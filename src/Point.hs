module Point (Point, fromInt, fromIntPartial, fromText, x, y) where

import Universum
import Flow

import Data.List (elemIndex)

-- | Represents a coordinate point on a board
newtype Point = Point (Int, Int) deriving (Show, Eq, Ord)
--newtype Point = Point Word8 deriving (Show, Eq, Ord)

-- | Create a Point, checking that x and y are in [0..14]
fromInt :: Int -> Int -> Maybe Point
fromInt x y
    | validate x && validate y =
        (x, y)
        |> Point
        |> Just 
    | otherwise = Nothing
    where
        validate x = x `elem` [0..14] -- GHC optimises this away, right?

-- don't use this at home
fromIntPartial :: Int -> Int -> Point
fromIntPartial x y = fromMaybe (error "invalid x or y") (fromInt x y)

-- | Another way to create a Point
fromText :: Text -> Maybe Point
fromText t =
    case toList t of
        (xCoord:yCoord) -> do
            x <- xCoord `elemIndex` "abcdefghijklmno"
            y <- readMaybe yCoord
            fromInt x (y-1) 
        _ -> Nothing 

-- | Get the X coordinate of a point
x :: Point -> Int
x (Point p) = fst p
-- x (Point p) = p `mod` 15 |> fromIntegral

-- | Get the Y coordinate of a point
y :: Point -> Int
y (Point p) = snd p
-- y (Point p) = p `div` 15 |> fromIntegral
