{-# LANGUAGE TemplateHaskell, LambdaCase, ViewPatterns #-}
module MoveSet where

import DefaultImports
import LitoUtils

import Move(Move)
import qualified Move
import Data.Aeson
import Data.Aeson.Types (toJSONKeyText)
import Data.Text (snoc)
import qualified Data.HashMap.Strict as HashMap
import Data.List.Index (imap)

-- Represents a renju position, does not preserve move order. Use MoveSeq if order is important.
data MoveSet = MoveSet {
    _moves :: HashMap Move Stone,
    _nextColor :: Stone
} deriving Show

data Stone
    = Black
    | White
    deriving (Show, Eq, Ord, Enum)

swapStone :: Stone -> Stone
swapStone Black = White
swapStone White = Black

makeLenses 'MoveSet

instance Eq MoveSet where
    (==) = (==) `on` longHashM

-- | Creates an Integer 'hash' of a position. longHash does not respect rotation / mirroring 
longHash :: MoveSet -> Integer
longHash =
    view moves
    .> HashMap.foldrWithKey' addHashPart 0
    where
        addHashPart :: Move -> Stone -> Integer -> Integer
        addHashPart move stone result = result + mult stone * Move.hashPart move
        
        mult Black = 1
        mult White = 2

-- | Applies a given transformation (rotation or mirroring) to a MoveSet
transform :: (Move -> Move) -> MoveSet -> MoveSet
transform f =
    over moves <| HashMap.mapKeys f

longHashM :: MoveSet -> Integer
longHashM ms =
    Move.transformations
    <&> flip transform ms
    <&> longHash
    |>  minimum

instance Hashable MoveSet where
    hashWithSalt salt = hashWithSalt salt <. longHashM

instance ToJSON MoveSet where
    toJSON = String <. toGetpos

instance ToJSONKey MoveSet where
    toJSONKey = toJSONKeyText toGetpos
instance FromJSON MoveSet where
    parseJSON = withText "MoveSet" parser where
        parser =
            fromGetpos
            .> \case
                Nothing -> fail "Parsing MoveSet failed"
                Just ms -> pure ms

instance FromJSONKey MoveSet where
    fromJSONKey = FromJSONKeyTextParser parser where
        parser (fromGetpos -> Just ms) = pure ms
        parser k = fail <| "cannot parse key " <> show k <> " into MoveSet"

empty :: MoveSet
empty = MoveSet HashMap.empty Black

isEmpty :: MoveSet -> Bool
isEmpty = (==MoveSet.empty)

moveCount :: MoveSet -> Int
moveCount =
    view moves
    .> length

stoneAt :: Move -> MoveSet -> Maybe Stone
stoneAt m =
    view moves
    .> HashMap.lookup m

notIn :: Move -> MoveSet -> Bool
notIn move =
    view moves
    .> (not <. HashMap.member move)

-- | Add a move with given coordinates to the position; if the coordinates are taken, return Nothing
makeMove :: Move -> MoveSet -> Maybe MoveSet
makeMove move ms
    | move `notIn` ms = ms
        |> moves %~ HashMap.insert move (ms ^. nextColor)
        |> nextColor %~ swapStone
        |> Just
    | otherwise = Nothing

-- | Like 'makeMove', but returns the same position if the coordinates are taken
makeMove' :: Move -> MoveSet -> MoveSet
makeMove' move =
    tryApply (makeMove move)

-- | Remove the given move from a MoveSet
remove :: Move -> MoveSet -> MoveSet
remove m = 
    over moves (HashMap.delete m)
    .> over nextColor swapStone


fromGetpos :: Text -> Maybe MoveSet
fromGetpos =
    (`snoc` 'a') .> foldl' f ("", [])
    .> snd
    .> sequence
    <.>> foldr' makeMove' MoveSet.empty
    where

    f :: (Text, [Maybe Move]) -> Char -> (Text, [Maybe Move])
    f (acc, moves') c
        | isCoordLetter c = (one c, if acc /= "" then Move.fromText acc : moves' else moves') -- if we encounter a char, try to parse the current accumulator to Move, then append the result to moves. Reset the accumulator.
        | otherwise = (acc `snoc` c, moves')                                                 -- if we encounter a digit, add it to the accumulator.

    isCoordLetter c = c >= 'a' && c <= 'o'

toGetpos :: MoveSet -> Text
toGetpos =
    view moves
    .> partition (==Black)
    .> over _1 keys
    .> over _2 keys
    .> uncurry alternateBlackAndWhite
    where
        alternateBlackAndWhite = go ..> map Move.toText ..> reverse ..> fold

        go :: [Move] -> [Move] -> [Move]
        go [x] [] = [x]
        go [] _ = error "length mismatch"
        go (x:xs) ys = x : go ys xs

-- | Apply a function to each Move that is not present in a given position
mapEmpty :: (Move -> a) -> MoveSet -> [a]
mapEmpty f ms =
    [0..224]
    <&> Move.fromBytePartial
     &  filter (`notIn` ms)
    <&> f

-- | Apply a function to all positions that can be derived from the current one
mapNext :: (MoveSet -> a) -> MoveSet -> [a]
mapNext f ms = mapEmpty (f <. (`MoveSet.makeMove'` ms)) ms

-- | Returns all parent positions (current one minus one move)
allPrev :: MoveSet -> [MoveSet]
allPrev (isEmpty -> True) = []
allPrev ms = ms 
    |> view moves
    |> HashMap.filter (/= view nextColor ms)
    |> keys
    |> map (`remove` ms)

toText :: (Move -> Maybe Stone -> Text) -> MoveSet -> Text
toText f ms =
    [0..14]
    <&> (\y -> (`Move.fromIntPartial` y) <$> [0..14])
    <&> map (f <*> flip MoveSet.stoneAt ms) -- S-combinator, OwO (this is similar to \ m -> f m (MoveSet.stoneAt m ms))
    |> imap (\i -> foldl' (<>) <| align <| i + 1)
    |> (letters :)
    |> reverse
    |> unlines
    where
        align i
            | i > 9 = show i
            | otherwise = " " <> show i

        letters = "   a b c d e f g h i j k l m n o" 