module Move (Move, fromInt, fromIntPartial, fromText, x, y) where

import Universum
import Flow

import Data.List (elemIndex)

-- | Represents a coordinate point on a board
newtype Move = Move (Int, Int) deriving (Show, Eq, Ord)
--newtype Move = Point Word8 deriving (Show, Eq, Ord)

-- | Create a Move, checking that x and y are in [0..14]
fromInt :: Int -> Int -> Maybe Move
fromInt x y
    | validate x && validate y =
        (x, y)
        |> Move
        |> Just 
    | otherwise = Nothing
    where
        validate x = x `elem` [0..14] -- GHC optimises this away, right?

-- don't use this at home
fromIntPartial :: Int -> Int -> Move
fromIntPartial x y = fromMaybe (error "invalid x or y") (fromInt x y)

-- | Another way to create a Move
fromText :: Text -> Maybe Move
fromText t =
    case toList t of
        (xCoord:yCoord) -> do
            x <- xCoord `elemIndex` "abcdefghijklmno"
            y <- readMaybe yCoord
            fromInt x (y-1) 
        _ -> Nothing 

-- | Get the X coordinate of a Move
x :: Move -> Int
x (Move p) = fst p
-- x (Move p) = p `mod` 15 |> fromIntegral

-- | Get the Y coordinate of a Move
y :: Move -> Int
y (Move p) = snd p
-- y (Move p) = p `div` 15 |> fromIntegral
