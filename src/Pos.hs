module Pos (Move(..), Pos, Pos.empty, unwrap, adjust, update, moveAt, moveCount, makeMove, makeMove', fromList, fromGetpos, Pos.toText, printPos) where

import Universum
import Flow
import LitoUtils

import Point (Point)
import qualified Point

import Data.Sequence ((!?))
import qualified Data.Sequence as Seq
import Data.Text (pack, unpack)

-- | Represents a move on the board
data Move
    = None
    | Black
    | White
    deriving (Show, Eq, Ord, Enum)

-- | A 15x15 matrix of moves. Gimme dependent types...
newtype Pos = Pos (Seq (Seq Move)) deriving Show -- I should a type different from Seq for this
                                                 -- Seq seems to be a decent choice after all. The other option is Vector
                                                 -- y then x, not the other way around!
instance Eq Pos where -- TODO: mirrored positions
    (Pos p1) == (Pos p2) = p1 == p2

toInteger :: Pos -> Integer
toInteger =
    unwrap
    .> join
    .> Seq.foldrWithIndex (\i move acc -> acc + 3^i * (fromEnum .> fromIntegral) move) 0

instance Ord Pos where
    compare = compare `on` Pos.toInteger

-- instance Hashable Position where -- if I decide to use a HashMap instead
    -- the simplest way to do this would be to hash that huge toInteger (~45 bytes) used for comparison
    -- UPD: turns out Integer is hashable

-- | Represents an empty board
empty :: Pos
empty =
    None
    |> Seq.replicate 15
    |> Seq.replicate 15
    |> Pos

unwrap :: Pos -> Seq (Seq Move)
unwrap (Pos p) = p

-- | An fmap-like function that unwraps a Position and wraps it back, not exported
fmapPos :: (Seq (Seq Move) -> Seq (Seq Move)) -> (Pos -> Pos)
fmapPos f =
    unwrap
    .> f
    .> Pos

-- | Apply 'f' to a move at given coordinates
adjust :: (Move -> Move) -> Point -> Pos -> Pos
adjust f point =
    Seq.adjust' (Seq.adjust' f (Point.x point)) (Point.y point)
    |> fmapPos

-- | Replace a move at given coordinates
update :: Move -> Point -> Pos -> Pos
update move xy =
    Seq.adjust' (Seq.update (Point.x xy) move) (Point.y xy)
    |> fmapPos

moveCount :: Pos -> Int
moveCount =
    unwrap   -- Pos -> Seq (Seq Move)
    <.>> fmap (\m -> if m == None then 0 else 1)
    <.>> sum
     .>  sum

moveAt :: Point -> Pos -> Move
moveAt xy (Pos p) = p `Seq.index` Point.y xy `Seq.index` Point.x xy -- using unsafe functions because Pos is validated on construction

-- | Add a move with given coordinates to the position; if the coordinates are taken, return Nothing
makeMove :: Point -> Pos -> Maybe Pos
makeMove point pos
    | moveAt point pos == None = update nextColor point pos |> Just
    | otherwise = Nothing
    where
        nextColor = if even <| moveCount pos then Black else White

-- | Like 'makeMove', but returns the same position if the coordinates are taken
makeMove' :: Point -> Pos -> Pos
makeMove' xy p = fromMaybe p <| makeMove xy p

-- | Make a Position from a list of lists. Validates only the board size, not the amount of black/white stones
fromList :: [[Move]] -> Maybe Pos
fromList moves =
    if all (length .> (==15)) moves then
        Nothing
    else
        moves
        <&> Seq.fromList -- I feel like I need a Flow equivalent of <&> (reversed fmap); maybe |$> will do?
        |>  Seq.fromList
        |>  Pos
        |> Just

fromGetpos :: Text -> Maybe Pos
fromGetpos = (<> "a") .> foldl' f ("", []) .> snd .> sequence <.>> magic where

    f :: (String, [Maybe Point]) -> Char -> (String, [Maybe Point])
    f (acc, moves) c
        | c >= 'a' && c <= 'o' = ([c], if acc /= "" then Point.fromText (pack acc) : moves else moves)       -- if we encounter a char, try to parse the current accumulator to Point, then append the result to moves. Reset the accumulator.
        | otherwise = (acc ++ [c], moves)                                                                    -- if we encounter a digit, add it to the accumulator.

    magic :: [Point] -> Pos
    magic = -- slow-ish conversion from a list of moves to a Pos
        foldr' makeMove' Pos.empty


-- I lowkey feel proud for implementing <.>> 

-- | A helper function for printPos
toText :: (Point -> Move -> Text) -> Pos -> Text
toText f =
    unwrap
    .> mapxy (\x y -> f <| fromJust <| Point.fromInt x y ) -- gib deptypes -- I could have proved these indices are always valid
    .> Seq.mapWithIndex (\i -> foldl' (<>) <| align <| i + 1)
    .> Seq.reverse
    .> (Seq.|> letters)
    .> toList
    .> unlines
    where
        align i
            | i > 9 = show i
            | otherwise = " " <> show i

        letters = "   a b c d e f g h i j k l m n o"

        fromJust (Just x) = x -- eww
        fromJust Nothing = error "bruh"

-- | Pretty-print a position
printPos :: Pos -> IO ()
printPos =
    Pos.toText moveToChar .> putText
    where
        moveToChar _ None  = " ."
        moveToChar _ Black = " x"
        moveToChar _ White = " o"