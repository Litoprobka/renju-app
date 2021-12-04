module MoveSeq where

import Universum
import Flow
import LitoUtils

import Move(Move)
import qualified Move

-- A different implementation of Pos. Preserves move order, so it can be used in Lib; should be faster to hash as well.
newtype MoveSeq = MoveSeq [Move] deriving Show -- even though the name is MoveSeq, I'm using List under the hood, since random access is not important
                                               -- this representation works only for Renju/Gomoku (i.e. not Pente), because it assumes every odd move is black and every even move is white

instance Eq MoveSeq where
    (==) = (==) `on` longHashM

-- | Creates an Integer 'hash' of a position. longHash does not respect rotation / mirroring 
longHash :: MoveSeq -> Integer
longHash =
    unwrap
    .> foldr' addHashPart (1, 0)
    .> snd
    where
        addHashPart :: Move -> (Integer, Integer) -> (Integer, Integer)
        addHashPart move (mult, result) =
            (if mult == 1 then 2 else 1, mult * Move.hashPart move)

-- compare this to Pos.transform
transform :: (Move -> Move) -> MoveSeq -> MoveSeq
transform f =
    unwrap
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

unwrap :: MoveSeq -> [Move]
unwrap (MoveSeq ms) = ms

-- adjust / update functions are not needed.

moveCount :: MoveSeq -> Int
moveCount =
    unwrap
    .> length

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
