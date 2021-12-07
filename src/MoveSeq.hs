{-# LANGUAGE LambdaCase #-}
module MoveSeq where

import Universum
import Flow
import LitoUtils

import Pos(Stone(..), fromMoveList, toText)
import Move(Move)
import qualified Move
import Data.List (elemIndex)

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

-- compare this to Pos.transform
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

stoneAt :: Move -> MoveSeq -> Stone
stoneAt move =
    getMoves
    .> elemIndex move
    .> \case
        Nothing -> None
        Just i | odd i -> Black
               | otherwise -> White

-- | Add a move with given coordinates to the position; if the coordinates are taken, return Nothing
makeMove :: Move -> MoveSeq -> Maybe MoveSeq
makeMove move (MoveSeq moves)
    | move `notElem` moves = Just <| MoveSeq <| move : moves
    | otherwise = Nothing

-- | Like 'makeMove', but returns the same position if the coordinates are taken
makeMove' :: Move -> MoveSeq -> MoveSeq
makeMove' move =
    tryApply (makeMove move)

fromGetpos :: Text -> Maybe MoveSeq
fromGetpos = fromGetpos' <.>> MoveSeq

-- | Apply a function to each Move that is not present in a given position
mapEmpty :: (Move -> a) -> MoveSeq -> [a]
mapEmpty f moves =
    [0..224]
    <&> Move.fromBytePartial
     &  filter (not . (`elem` getMoves moves))
    <&> f

-- | Apply a function to all positions that can be derived from the current one
mapNext :: (MoveSeq -> a) -> MoveSeq -> [a]
mapNext f moves = mapEmpty (f <. (`MoveSeq.makeMove'` moves)) moves

toText :: (Move -> Stone -> Text) -> MoveSeq -> Text
toText f moves =
    Pos.toText f <| fromMoveList <| getMoves <| moves