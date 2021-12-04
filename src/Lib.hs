module Lib where

import Universum
import Flow

import Move (Move)
import qualified Move

import Pos (Pos, Stone(..), moveCount)
import qualified Pos

import qualified Data.Sequence as Seq
import qualified Data.HashMap.Strict as HashMap
import Data.Set (powerSet)

-- | Additional info for a position, such as comments
newtype MoveInfo = MoveInfo {
    comment :: Text
    -- other stuff like board text, previous move (if needed)
}

-- instance Default MoveInfo where 
defMoveInfo = MoveInfo ""

type LibLayer = HashMap Pos MoveInfo
-- | Represents a database file
data Lib = Lib {
      lib :: Seq LibLayer
    , moves :: [Move]
}

-- | Represents an empty database
empty :: Lib
empty =
    Lib (Seq.replicate 225 HashMap.empty) [] 

back :: Lib -> Lib
back l@(Lib _ []) = l
back l@(Lib _ (m:ms)) = l { moves = ms }

-- | add a position to the lib
addPos :: Pos -> Lib -> Lib
addPos pos l =
    l { lib = lib l |> Seq.adjust (HashMap.insert pos defMoveInfo) (moveCount pos) } -- seems like it's time to learn lens

-- | add a move to the lib
addMove :: Move -> Lib -> Lib
addMove move l@(Lib lib moves)
    | move `elem` moves = l -- if the current position already has this move, do nothing
    | otherwise = addPos pos l { moves = newMoves }
    where
        newMoves = move : moves
        pos = Pos.fromMoveList newMoves

-- | removes a position from the lib
removePos :: Pos -> Lib -> Lib
removePos pos l =
    l { lib = lib l |> Seq.adjust (HashMap.delete pos) (moveCount pos) }

-- | removes current position from the lib
remove :: Lib -> Lib
remove l@(Lib _ []) = l
remove l = l |> removePos (Pos.fromMoveList <| moves l) |> back

nextMoveHelper :: (Pos -> LibLayer -> a) -> Move -> Pos -> Lib -> a
nextMoveHelper f move pos l = lib l `Seq.index` Pos.moveCount newPos |> f newPos where
    newPos = Pos.makeMove' move pos 

nextMoveExists :: Move -> Pos -> Lib -> Bool -- not sure about the argument order, maybe Move and Pos should be the other way around
nextMoveExists = nextMoveHelper HashMap.member

getNextMove :: Move -> Pos -> Lib -> Maybe MoveInfo
getNextMove = nextMoveHelper HashMap.lookup

printLib :: Lib -> IO ()
printLib lib =
    pos
    |> Pos.toText char
    |> putText
    where
        pos = Pos.fromMoveList <| moves lib

        char p None  = if nextMoveExists p pos lib then " +" else " ."
        char _ Black = " x"
        char _ White = " o"

-- some UI-related functions

transform :: (Move -> Move) -> Lib -> Lib
transform f l = l { moves = moves l <&> f }

mirror :: Lib -> Lib
mirror = transform (\ p -> Move.fromIntPartial (14 - Move.x p) (Move.y p))

rotate :: Lib -> Lib
rotate = transform (\ p -> Move.fromIntPartial (Move.y p) (14-Move.x p))