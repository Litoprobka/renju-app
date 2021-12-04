module Pos (Stone(..), Pos, Pos.empty, transform, longHash, longHashM, unwrap, adjust, update, moveAt, moveCount, makeMove, makeMove', fromMoveList, fromStoneList, fromGetpos, Pos.toText, printPos) where

import Universum
-- import Universum.Container
import Flow
import LitoUtils

import Move (Move)
import qualified Move

import qualified Data.Sequence as Seq
import qualified Data.Tree as Seq

-- | Represents a stone
data Stone
    = None
    | Black
    | White
    deriving (Show, Eq, Ord, Enum)

-- | A 15x15 matrix of moves. Gimme dependent types...
newtype Pos = Pos (Seq (Seq Stone)) deriving Show    -- I should a type different from Seq for this
                                                    -- Seq seems to be a decent choice after all. The other option is Vector
                                                    -- y then x, not the other way around!

instance Eq Pos where
    (==) = (==) `on` longHashM

-- | Creates an Integer 'hash' of a position.  longHash pos /= longHash (mirror Pos) 
longHash :: Pos -> Integer
longHash =
    unwrap
    .> join
    .> Seq.foldrWithIndex (\i move acc -> acc + 3^i * (fromEnum .> fromIntegral) move) 0

-- slow af
transform :: (Int -> Int -> Move) -> Pos -> Pos
transform f =
    unwrap 
    .> mapxy (\x y s -> (s, f x y))
    .> join
    .> Seq.filter (fst .> (/=None))
    .> Seq.partition (fst .> (==Black))
    .> \ (black, white) ->
        let posWithBlack = foldr' (update Black) Pos.empty (snd <$> black)
        in foldr' (update White) posWithBlack (snd <$> white)

-- I feel so-o-o proud for figuring this out
-- | Computes longHash of pos and its 7 possible transfomations, then takes the smallest one (i.e. it respects mirroring)
longHashM :: Pos -> Integer
longHashM p =
    transformations      -- [Int -> Int -> Move]
    <&> flip transform p -- [Pos]
    <&> longHash         -- [Integer]
    |>  minimum
    where
        f = Move.fromIntPartial
        transformations :: NonEmpty (Int -> Int -> Move)
        transformations = fromMaybe (error "impossible") <| nonEmpty [ -- dependent types...
            f
            , flip f
            , \x y -> f x (14-y)
            , \x y -> f y (14-x)
            , \x y -> f (14-x) y
            , \x y -> f (14-y) x
            , \x y -> f (14-x) (14-y)
            , \x y -> f (14-y) (14-x)
            ] 

instance Hashable Pos where
    hashWithSalt salt = hashWithSalt salt <. longHashM

-- | Represents an empty board
empty :: Pos
empty =
    None
    |> Seq.replicate 15
    |> Seq.replicate 15
    |> Pos

unwrap :: Pos -> Seq (Seq Stone)
unwrap (Pos p) = p

-- | An fmap-like function that unwraps a Position and wraps it back, not exported
fmapPos :: (Seq (Seq Stone) -> Seq (Seq Stone)) -> (Pos -> Pos)
fmapPos f =
    unwrap
    .> f
    .> Pos

-- | Apply 'f' to a move at given coordinates
adjust :: (Stone -> Stone) -> Move -> Pos -> Pos
adjust f move =
    Seq.adjust' (Seq.adjust' f (Move.x move)) (Move.y move)
    |> fmapPos

-- | Replace a move at given coordinates
update :: Stone -> Move -> Pos -> Pos
update move xy =
    Seq.adjust' (Seq.update (Move.x xy) move) (Move.y xy)
    |> fmapPos

moveCount :: Pos -> Int
moveCount =
    unwrap   -- Pos -> Seq (Seq Stone)
    <.>> fmap (\m -> if m == None then 0 else 1)
    <.>> sum
     .>  sum

moveAt :: Move -> Pos -> Stone
moveAt xy (Pos p) = p `Seq.index` Move.y xy `Seq.index` Move.x xy -- using unsafe functions because Pos is validated on construction

-- | Add a move with given coordinates to the position; if the coordinates are taken, return Nothing
makeMove :: Move -> Pos -> Maybe Pos
makeMove move pos
    | moveAt move pos == None = update nextColor move pos |> Just
    | otherwise = Nothing
    where
        nextColor = if even <| moveCount pos then Black else White

-- | Like 'makeMove', but returns the same position if the coordinates are taken
makeMove' :: Move -> Pos -> Pos
makeMove' xy p = fromMaybe p <| makeMove xy p

-- | Make a Position from a list of lists. Validates only the board size, not the amount of black/white stones
fromStoneList :: [[Stone]] -> Maybe Pos
fromStoneList moves =
    if all (length .> (==15)) moves then
        Nothing
    else
        moves
        <&> Seq.fromList -- I feel like I need a Flow equivalent of <&> (reversed fmap); maybe |$> will do?
        |>  Seq.fromList
        |>  Pos
        |> Just
-- slow-ish conversion from a list of Moves to a Pos
fromMoveList :: [Move] -> Pos
fromMoveList = foldr' makeMove' Pos.empty

fromGetpos :: Text -> Maybe Pos
fromGetpos = (<> "a") .> foldl' f ("", []) .> snd .> sequence <.>> fromMoveList where

    f :: (String, [Maybe Move]) -> Char -> (String, [Maybe Move])
    f (acc, moves) c
        | c >= 'a' && c <= 'o' = ([c], if acc /= "" then Move.fromText (Universum.toText acc) : moves else moves)       -- if we encounter a char, try to parse the current accumulator to Move, then append the result to moves. Reset the accumulator.
        | otherwise = (acc ++ [c], moves)                                                                    -- if we encounter a digit, add it to the accumulator.


-- I lowkey feel proud for implementing <.>> 

-- | A helper function for printPos
toText :: (Move -> Stone -> Text) -> Pos -> Text
toText f =
    unwrap
    .> mapxy (\x y -> f <| Move.fromIntPartial x y ) -- gib deptypes -- I could have proved these indices are always valid
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

-- | Pretty-print a position
printPos :: Pos -> IO ()
printPos =
    Pos.toText moveToChar .> putText
    where
        moveToChar _ None  = " ."
        moveToChar _ Black = " x"
        moveToChar _ White = " o"