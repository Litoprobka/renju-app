{-# LANGUAGE TemplateHaskell, LambdaCase, ViewPatterns, GeneralisedNewtypeDeriving #-}
module MoveSeq where

import DefaultImports
import LitoUtils

import Move(Move)
import qualified Move
import Data.List (elemIndex)
import Data.List.Index (imap, ipartition, deleteAt)
import Data.Aeson
import Data.Aeson.Types (toJSONKeyText)
import Data.Text (snoc)
import Lens.Micro (to, SimpleGetter)
import qualified Data.List.NonEmpty as NonEmpty

-- A different implementation of Pos. Preserves move order, so it can be used in Lib; should be faster to hash as well.
data MoveSeq = MoveSeq { _moveList :: [Move], _nextColor :: Stone, _hashes :: NonEmpty LongHash } deriving Show -- even though the name is MoveSeq, I'm using List under the hood, since random access is not important
                                               -- this representation works only for Renju/Gomoku (i.e. not Pente), because it assumes every odd move is black and every even move is white
newtype LongHash = LongHash Integer deriving (Show, Eq, Ord, Num, Hashable)

data Stone
    = Black
    | White
    deriving (Show, Eq, Ord, Enum)

makeLenses 'MoveSeq
hash :: SimpleGetter MoveSeq LongHash
hash = to <| minimum <. view hashes
instance Eq MoveSeq where
    (==) = (==) `on` view hash

instance Hashable MoveSeq where
    hashWithSalt salt = hashWithSalt salt <. view hash

instance ToJSON MoveSeq where
    toJSON = String <. toGetpos

instance ToJSONKey MoveSeq where
    toJSONKey = toJSONKeyText toGetpos
instance FromJSON MoveSeq where
    parseJSON = withText "MoveSeq" parser where
        parser =
            fromGetpos
            .> \case
                Nothing -> fail "Parsing MoveSeq failed"
                Just ms -> pure ms

instance FromJSONKey MoveSeq where
    fromJSONKey = FromJSONKeyTextParser parser where
        parser (fromGetpos -> Just ms) = pure ms
        parser k = fail <| "cannot parse key " <> show k <> " into MoveSeq"

-- | /O(1)/ A MoveSeq with no moves.
empty :: MoveSeq
empty = MoveSeq [] Black (z :| replicate 7 z) where
    z = LongHash 0

-- | /O(1)/ Checks if a MoveSeq is empty
isEmpty :: MoveSeq -> Bool
isEmpty MoveSeq{_moveList = []} = True
isEmpty _ = False

flipStone :: Stone -> Stone
flipStone Black = White
flipStone White = Black

hashMult :: Num a => Stone -> a
hashMult Black = 1
hashMult White = 2

flipNextColor :: MoveSeq -> MoveSeq
flipNextColor = over nextColor flipStone

updateHashes :: (LongHash -> LongHash -> LongHash) -> (Stone -> Integer) -> Move -> MoveSeq -> MoveSeq
updateHashes f mult move ms = ms
    |> over hashes (NonEmpty.zipWith zf Move.transformations)
    where
    zf trns = flip f (fromIntegral <| (* mult (ms ^. nextColor)) <| Move.hashPart <| trns move)

-- | /O(1)/ Returns the positon without the last move, or the same position if it was empty
back :: MoveSeq -> MoveSeq
back ms = case view moveList ms of
    [] -> ms
    m : moves -> ms
        |> set moveList moves
        |> updateHashes (-) (flipStone .> hashMult) m
        |> flipNextColor

-- | /O(n)/
moveCount :: MoveSeq -> Int
moveCount =
    view moveList
    .> length

-- | /O(n)/ Returns the index of a move in a position, counting from one; Nothing if the does not exist in the position
moveIndex :: Move -> MoveSeq -> Maybe Int
moveIndex move =
    view moveList
    .> reverse
    .> elemIndex move
    <.>> (+1)

-- | /O(n)/ Returns the stone color of a move in a position, or Nothing if it does not contain the move
stoneAt :: Move -> MoveSeq -> Maybe Stone
stoneAt move =
    moveIndex move
    <.>> \case
        (odd -> True) -> Black
        _ -> White

-- | /O(n)/ Checks if a move does not in a position
notIn :: Move -> MoveSeq -> Bool
notIn move = stoneAt move .> isNothing

-- | /O(1)/ Add a move with given coordinates to the position; if the coordinates are taken, return Nothing
makeMove :: Move -> MoveSeq -> Maybe MoveSeq
makeMove move pos
    | move `notIn` pos = pos
        |> over moveList (move :)
        |> updateHashes (+) hashMult move
        |> flipNextColor
        |> Just
    | otherwise = Nothing

-- | /O(1)/ Like 'makeMove', but returns the same position if the coordinates are taken
makeMove' :: Move -> MoveSeq -> MoveSeq
makeMove' move =
    tryApply (makeMove move)

-- | /O(n)/ Constructs a MoveSeq from a list of moves
fromList :: [Move] -> MoveSeq
fromList = foldr' makeMove' MoveSeq.empty

-- | /O(n)/ Constructs a MoveSeq from a string in getpos format ("h8i9j6i8k8")
fromGetpos :: Text -> Maybe MoveSeq
fromGetpos =
    (`snoc` 'a') .> foldl' f ("", [])
    .> snd
    .> sequence
    <.>> fromList
    where

    f :: (Text, [Maybe Move]) -> Char -> (Text, [Maybe Move])
    f (acc, moves) c
        | isCoordLetter c = (one c, if acc /= "" then Move.fromText acc : moves else moves) -- if we encounter a char, try to parse the current accumulator to Move, then append the result to moves. Reset the accumulator.
        | otherwise = (acc `snoc` c, moves)                                                 -- if we encounter a digit, add it to the accumulator.

    isCoordLetter c = c >= 'a' && c <= 'o'

-- | /O(n)/ Convert a position to getpos format
toGetpos :: MoveSeq -> Text
toGetpos =
    view moveList
    <.>> Move.toText
    .> reverse
    .> fold

-- | /~O(n^2)/ Apply a function to each Move that is not present in a given position.
-- 
-- Technically, complexity is linear (225n), but in practice it is worse than /O(n^2)/, because n <= 225
mapEmpty :: (Move -> a) -> MoveSeq -> [a]
mapEmpty f moves =
    [0..224]
    <&> Move.fromBytePartial
     &  filter (`notIn` moves)
    <&> f

-- | /~O(n^2)/ Apply a function to all positions that can be derived from the current one
mapNext :: (MoveSeq -> a) -> MoveSeq -> [a]
mapNext f moves = mapEmpty (f <. (`MoveSeq.makeMove'` moves)) moves

-- | /O(n^2)/ Returns all parent positions (current one minus one move)
allPrev :: MoveSeq -> [MoveSeq]
allPrev MoveSeq{ _moveList = [] } = []
allPrev moves =
    moves ^. moveList
    |> reverse
    |> ipartition (\i _ -> even i) -- True for black moves, False for white moves; I could write this as (even .> const), but the explicit lambda is more readable
    |> _1 %~ copies
    |> _2 %~ copies
    |> blackOrWhite %~ imap deleteAt -- I love lens
    |> uncurry (zipWith toMoveSeq) -- figuring this out took quite a bit of time
    where
        copies = replicate <| (moveCount moves - 1) `div` 2 + 1 -- 4 -> 2, 5 -> 3, 17 -> 8...
        blackOrWhite = if moves ^. nextColor == White then _1 else _2

        toMoveSeq :: [Move] -> [Move] -> MoveSeq
        toMoveSeq b w
            | even <| moveCount moves = go b w |> fromList -- move count is even => last move was white => previous to last move was black => start with black
            | otherwise = go w b |> fromList
        go [x] [] = [x]
        go [] _ = error "length mismatch"
        go (x:xs) ys = x : go ys xs

-- | /O(n)/ Applies a given transformation (rotation or mirroring) to a MoveSeq
transform :: (Move -> Move) -> MoveSeq -> MoveSeq
transform f =
    over moveList (map f)

toText :: (Move -> Maybe Stone -> Text) -> MoveSeq -> Text
toText f ms =
    [0..14]
    <&> (\y -> (`Move.fromIntPartial` y) <$> [0..14])
    <&> map (f <*> flip MoveSeq.stoneAt ms) -- S-combinator, OwO (this is similar to \ m -> f m (MoveSeq.stoneAt m ms))
    |> imap (\i -> foldl' (<>) <| align <| i + 1)
    |> (letters :)
    |> reverse
    |> unlines
    where
        align i
            | i > 9 = show i
            | otherwise = " " <> show i

        letters = "   a b c d e f g h i j k l m n o" 