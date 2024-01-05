{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module Move where

import Data.List (elemIndex, (!!))
import Data.Vector (Vector)
import qualified Data.Vector.Generic.Sized as S
import DefaultImports

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

-- | Create a Move, checking that x and y are in [0..14]
fromInt :: Int -> Int -> Maybe Move
fromInt x' y'
  | validate x' && validate y' =
      Just <| Move x' y'
  | otherwise = Nothing
 where
  validate coord = coord `elem` [0 .. 14] -- GHC optimises this away, right?

{- | Create a Move, throwing an error if x or y are out of bounds.

All of my calls to this function are safe, but there is no way to prove it without dependent / refinement types
-}
fromIntPartial :: Int -> Int -> Move
fromIntPartial x' y' = fromMaybe (error "invalid x or y") (fromInt x' y')

charCoords :: String
charCoords = "abcdefghijklmno"

-- | Create a Move from getpos format, i.e. "i7" -> (9, 6)
fromText :: Text -> Maybe Move
fromText t =
  case toString t of
    (xCoord : yCoord) -> do
      x' <- xCoord `elemIndex` charCoords
      y' <- readMaybe yCoord
      fromInt x' (y' - 1)
    _ -> Nothing

-- | Create a Move from a number in /x + 15 * y/ format.
fromBytePartial :: Int -> Move
fromBytePartial i
  | i > 224 = error "invalid x or y"
  | otherwise = i `divMod` 15 |> swap .> uncurry Move

-- * Destruction

-- | Convert a move to getpos format, i.e. (9, 6) -> "i7"
toText :: Move -> Text
toText m =
  one (charCoords !! view x m) <> show (m ^. y + 1)

-- | Convert a Move to /x + 15 * y/. Used for hashing.
toByte :: Move -> Int
toByte m = m ^. x + m ^. y * 15

-- | Used to hash MoveSeq. A 'longhash' of MoveSeq is a sum of all hashParts of its moves, hence the name
hashPart :: Move -> Integer
hashPart m =
  3 ^ toByte m

-- * Other

-- | All mirroring and rotation functions that maintain the relative position of a move on the board
transformations :: Vec8 (Move -> Move)
transformations =
  S.fromTuple
    ( id
    , invert x
    , invert y
    , invert x .> invert y
    , swapxy
    , swapxy .> invert x
    , swapxy .> invert y
    , swapxy .> invert x .> invert y
    )
 where
  invert coord = over coord (14 -)
  swapxy (Move x' y') = Move y' x'
