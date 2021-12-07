{-# LANGUAGE TemplateHaskell #-}

module Move (Move, fromInt, fromIntPartial, fromText, fromBytePartial, hashPart, transformations, getX, getY) where

import Universum hiding (over, view, (^.))
import Control.Lens hiding ((.>), (<|), (|>))
import Flow

import Data.List (elemIndex)

-- | Represents a coordinate point on a board
data Move = Move 
    { _x :: Int
    , _y :: Int }
    deriving (Show, Eq, Ord)

makeLenses 'Move

-- | Create a Move, checking that x and y are in [0..14]
fromInt :: Int -> Int -> Maybe Move
fromInt x' y'
    | validate x' && validate y' =
        Just <| Move x' y'
    | otherwise = Nothing
    where
        validate coord = coord `elem` [0..14] -- GHC optimises this away, right?

-- don't use this at home
fromIntPartial :: Int -> Int -> Move
fromIntPartial x' y' = fromMaybe (error "invalid x or y") (fromInt x' y')

-- | Another way to create a Move
fromText :: Text -> Maybe Move
fromText t =
    case toList t of
        (xCoord:yCoord) -> do
            x' <- xCoord `elemIndex` "abcdefghijklmno"
            y' <- readMaybe yCoord
            fromInt x' (y'-1)
        _ -> Nothing

fromBytePartial :: Int -> Move
fromBytePartial i
    | i > 224 = error "invalid x or y"
    | otherwise = Move x' y' where
        (x', y') = i `divMod` 15
        

-- | Used to hash MoveSeq
hashPart :: Move -> Integer
hashPart m =
    3^(m^.x + m^.y * 15)

transformations :: NonEmpty (Move -> Move)
transformations = fromMaybe (error "impossible") <| nonEmpty [ -- dependent types...
        id
        , invert x
        , invert y
        , invert x .> invert y 
        , swapxy
        , swapxy .> invert x 
        , swapxy .> invert y  
        , swapxy .> invert x .> invert y 
    ]
    where
        invert coord = over coord (14-)
        swapxy (Move x' y') = Move y' x'

getX :: Move -> Int
getX = _x

getY :: Move -> Int
getY = _y
{-
getX :: Getter Move Int
getX = to _x

getY :: Getter Move Int
getY = to _y
-}
