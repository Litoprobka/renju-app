{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DerivingVia #-}

module Move where

import DefaultImports

import Data.List (elemIndex, (!!))
import Data.Vector (Vector)
import Data.Vector.Generic.Sized qualified as S
import GHC.Generics ( Generic1, Generically1(..) )

-- | Represents a coordinate point on a board
data Move = Move
  { _x :: Int
  , _y :: Int
  }
  deriving (Show, Eq, Ord)

type Vec8 = S.Vector Vector 8

makeLenses 'Move

instance Hashable Move where
  hashWithSalt salt = toByte .> (+ salt)

-- * Creation

{- | Create a Move, checking that x and y are in [0..14]

>>> fromInt 7 7 <&> Move.toText
Just "h8"
>>> fromInt 7 15 <&> Move.toText
Nothing
-}
fromInt :: Int -> Int -> Maybe Move
fromInt x' y'
  | validate x' && validate y' =
      Just <| Move x' y'
  | otherwise = Nothing
 where
  validate coord = coord `elem` [0 .. 14] -- GHC optimises this away, right?

charCoords :: String
charCoords = "abcdefghijklmno"

{- | Create a Move from getpos format, i.e. "i7" -> (9, 6)

>>> fromText "h9"
Just (Move {_x = 7, _y = 8})
-}
fromText :: Text -> Maybe Move
fromText t =
  case toString t of
    (xCoord : yCoord) -> do
      x' <- xCoord `elemIndex` charCoords
      y' <- readMaybe yCoord
      fromInt x' (y' - 1)
    _ -> Nothing

-- * Destruction

-- | Convert a move to getpos format, i.e. (9, 6) -> "i7"
toText :: Move -> Text
toText m =
  one (charCoords !! view x m) <> show (m ^. y + 1)

{- | Convert a Move to /x + 15 * y/. Used for hashing.

>>> toByte <$> fromText "i6"
Just 83
>>> toByte <$> fromText "a1"
Just 0
>>> toByte <$> fromText "o15"
Just 224
-}
toByte :: Move -> Int
toByte m = m ^. x + m ^. y * 15

-- | Used to hash MoveSeq. A 'longhash' of MoveSeq is a sum of all hashParts of its moves, hence the name
hashPart :: Move -> Natural
hashPart m =
  4 ^ toByte m

-- * Other

-- | A 15x15 grid of moves
grid :: [[Move]]
grid =
  [[Move x y | x <- [0 .. 14]] | y <- [0 .. 14]]

data Transformations a = Transformations
  { origin :: a
  , invertX :: a
  , invertY :: a
  , rotate180 :: a
  , swapXY :: a
  , rotateL :: a
  , rotateR :: a
  , swapInvertBoth :: a
  } deriving (Show, Functor, Foldable, Generic1)
    deriving Applicative via Generically1 Transformations

-- | All mirroring and rotation functions that maintain the relative position of a move on the board
transformations :: Transformations (Move -> Move)
transformations =
  Transformations {
    origin = id
    , invertX = invert x
    , invertY = invert y
    , rotate180 = invert x .> invert y
    , swapXY = swapxy
    , rotateL = swapxy .> invert x
    , rotateR = swapxy .> invert y
    , swapInvertBoth = swapxy .> invert x .> invert y
  }
 where
  invert coord = over coord (14 -)
  swapxy (Move x' y') = Move y' x'