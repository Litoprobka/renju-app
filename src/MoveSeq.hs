{-# LANGUAGE LambdaCase, ViewPatterns #-}
module MoveSeq where

import DefaultImports
import LitoUtils

import Move(Move)
import qualified Move
import Data.List (elemIndex)
import Data.List.Index (imap, ipartition, deleteAt)
import qualified Data.Text as Text (concat)
import Data.Aeson
import Data.Aeson.Types (toJSONKeyText)
import Data.Text (snoc)

-- A different implementation of Pos. Preserves move order, so it can be used in Lib; should be faster to hash as well.
newtype MoveSeq = MoveSeq { getMoves :: [Move] } deriving Show -- even though the name is MoveSeq, I'm using List under the hood, since random access is not important
                                               -- this representation works only for Renju/Gomoku (i.e. not Pente), because it assumes every odd move is black and every even move is white

instance Eq MoveSeq where
    (==) = (==) `on` longHashM

-- | Creates an Integer 'hash' of a position. longHash does not respect rotation / mirroring 
longHash :: MoveSeq -> Integer
longHash =
    getMoves
    .> foldr' addHashPart (1, 0)
    .> snd
    where
        addHashPart :: Move -> (Integer, Integer) -> (Integer, Integer)
        addHashPart move (mult, result) =
            (if mult == 1 then 2 else 1, result + mult * Move.hashPart move)

-- | Applies a given transformation (rotation or mirroring) to a MoveSeq
transform :: (Move -> Move) -> MoveSeq -> MoveSeq
transform f =
    getMoves
    <.>> f
    .> MoveSeq 

longHashM :: MoveSeq -> Integer
longHashM moves =
    Move.transformations
    <&> flip transform moves
    <&> longHash
    |>  minimum

instance Hashable MoveSeq where
    hashWithSalt salt = hashWithSalt salt <. longHashM

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

data Stone
    = None
    | Black
    | White
    deriving (Show, Eq, Ord, Enum)

empty :: MoveSeq
empty = MoveSeq []

isEmpty :: MoveSeq -> Bool
isEmpty = (==MoveSeq.empty)

-- adjust / update functions are not needed.

back :: MoveSeq -> MoveSeq
back (MoveSeq []) = MoveSeq []
back (MoveSeq (_ : ms)) = MoveSeq ms

moveCount :: MoveSeq -> Int
moveCount =
    getMoves
    .> length

moveIndex :: Move -> MoveSeq -> Maybe Int
moveIndex move =
    getMoves
    .> reverse
    .> elemIndex move
    <.>> (+1)

stoneAt :: Move -> MoveSeq -> Stone
stoneAt move =
    moveIndex move
    .> \case
        Nothing -> None
        Just i | odd i -> Black
               | otherwise -> White

notIn :: Move -> MoveSeq -> Bool
notIn move = stoneAt move .> (==None)

-- | Add a move with given coordinates to the position; if the coordinates are taken, return Nothing
makeMove :: Move -> MoveSeq -> Maybe MoveSeq
makeMove move pos@(MoveSeq moves)
    | move `notIn` pos = Just <| MoveSeq <| move : moves
    | otherwise = Nothing

-- | Like 'makeMove', but returns the same position if the coordinates are taken
makeMove' :: Move -> MoveSeq -> MoveSeq
makeMove' move =
    tryApply (makeMove move)

fromGetpos :: Text -> Maybe MoveSeq
fromGetpos = 
    (`snoc` 'a') .> foldl' f ("", [])
    .> snd
    .> sequence
    <.>> MoveSeq
    where

    f :: (Text, [Maybe Move]) -> Char -> (Text, [Maybe Move])
    f (acc, moves) c
        | isCoordLetter c = (one c, if acc /= "" then Move.fromText acc : moves else moves) -- if we encounter a char, try to parse the current accumulator to Move, then append the result to moves. Reset the accumulator.
        | otherwise = (acc `snoc` c, moves)                                                 -- if we encounter a digit, add it to the accumulator.

    isCoordLetter c = c >= 'a' && c <= 'o'

toGetpos :: MoveSeq -> Text
toGetpos =
    getMoves
    <.>> Move.toText
    .> reverse
    .> Text.concat

-- | Apply a function to each Move that is not present in a given position
mapEmpty :: (Move -> a) -> MoveSeq -> [a]
mapEmpty f moves =
    [0..224]
    <&> Move.fromBytePartial
     &  filter (`notIn` moves)
    <&> f

-- | Apply a function to all positions that can be derived from the current one
mapNext :: (MoveSeq -> a) -> MoveSeq -> [a]
mapNext f moves = mapEmpty (f <. (`MoveSeq.makeMove'` moves)) moves

-- | Returns all parent positions (current one minus one move)
allPrev :: MoveSeq -> [MoveSeq]
allPrev (MoveSeq []) = []
allPrev moves =
    getMoves moves
    |> ipartition (\i _ -> even i) -- True for black moves, False for white moves; I could write this as (even .> const), but the explicit lambda is more readable
    |> _1 %~ copies
    |> _2 %~ copies
    |> blackOrWhite %~ imap deleteAt -- I love lens
    |> uncurry (zipWith toMoveSeq) -- figuring this out took quite a bit of time
    where 
        copies = replicate <| (moveCount moves - 1) `div` 2 + 1 -- 4 -> 2, 5 -> 3, 17 -> 8...
        blackOrWhite = if odd (moveCount moves) then _1 else _2 -- move count is odd => last move was black => try removing black moves
        
        toMoveSeq :: [Move] -> [Move] -> MoveSeq
        toMoveSeq b w
            | even <| moveCount moves = go b w |> MoveSeq -- move count is even => last move was white => previous to last move was black => start with black
            | otherwise = go w b |> MoveSeq
        go [x] [] = [x]
        go [ ] _ = error "length mismatch"
        go (x:xs) ys = x : go ys xs

toText :: (Move -> Stone -> Text) -> MoveSeq -> Text
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