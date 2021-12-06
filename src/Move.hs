module Move (Move, fromInt, fromIntPartial, fromText, fromBytePartial, hashPart, transformations, x, y) where

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

fromBytePartial :: Int -> Move
fromBytePartial i
    | i > 224 = error "invalid x or y"
    | otherwise =
        i `divMod` 15
        |> swap
        |> Move

-- | Used to hash MoveSeq
hashPart :: Move -> Integer
hashPart m =
    3^(x m + y m * 15)

transformations :: NonEmpty (Move -> Move)
transformations = map (\f (Move m) -> Move <| f m) <| fromMaybe (error "impossible") <| nonEmpty [ -- dependent types...
    id
    , first (14 -)
    , second (14 -)
    , bimap (14 -) (14 -)
    , swap
    , swap .> first (14 -)
    , swap .> second (14 -)
    , swap .> bimap (14 -) (14 -)
    ]

-- | Get the X coordinate of a Move
x :: Move -> Int
x (Move m) = fst m
-- x (Move m) = m `mod` 15 |> fromIntegral

-- | Get the Y coordinate of a Move
y :: Move -> Int
y (Move m) = snd m
-- y (Move m) = m `div` 15 |> fromIntegral
