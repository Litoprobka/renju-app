module Lib where

import Universum
import Flow

import Move (Move)
import qualified Move

import Pos (Pos, Stone(..))

import MoveSeq (MoveSeq)
import qualified MoveSeq

import qualified Data.Sequence as Seq
import qualified Data.HashMap.Strict as HashMap

-- | Additional info for a position, such as comments
newtype MoveInfo = MoveInfo {
    comment :: Text
    -- other stuff like board text, previous move (if needed)
}

-- instance Default MoveInfo where 
defMoveInfo = MoveInfo ""

type LibLayer = HashMap MoveSeq MoveInfo
-- | Represents a database file
data Lib = Lib {
      lib :: Seq LibLayer
    , moves :: MoveSeq
}

-- | Represents an empty database
empty :: Lib
empty =
    Lib (Seq.replicate 225 HashMap.empty) MoveSeq.empty

back :: Lib -> Lib
back l = l { moves = MoveSeq.back <| moves l }

-- | add a position to the lib
addPos :: MoveSeq -> Lib -> Lib
addPos pos l
    | pos `HashMap.member` (lib l `Seq.index` MoveSeq.moveCount pos) = l
    | otherwise =
        l { lib = lib l |> Seq.adjust (HashMap.insert pos defMoveInfo) (MoveSeq.moveCount pos) } -- seems like it's time to learn lens

-- | add a move to the lib
addMove :: Move -> Lib -> Lib
addMove move l =
    addPos pos l { moves = pos }
    where
        pos = MoveSeq.makeMove' move <| moves l

-- | removes a position from the lib
removePos :: MoveSeq -> Lib -> Lib
removePos pos l
    | MoveSeq.moveCount pos == 0 = l
    | otherwise = l { lib = lib l |> Seq.adjust (HashMap.delete pos) (MoveSeq.moveCount pos) }

-- | removes current position from the lib
remove :: Lib -> Lib
remove l = l |> removePos (moves l) |> back

nextMoveHelper :: (MoveSeq -> LibLayer -> a) -> Move -> MoveSeq -> Lib -> a
nextMoveHelper f move pos l = lib l `Seq.index` MoveSeq.moveCount newPos |> f newPos where
    newPos = MoveSeq.makeMove' move pos

nextMoveExists :: Move -> MoveSeq -> Lib -> Bool -- not sure about the argument order, maybe Move and Pos should be the other way around
nextMoveExists = nextMoveHelper HashMap.member

getNextMove :: Move -> MoveSeq -> Lib -> Maybe MoveInfo
getNextMove = nextMoveHelper HashMap.lookup

printLib :: Lib -> IO ()
printLib lib =
    pos
    |> MoveSeq.toText char
    |> putText
    where
        pos = moves lib

        char p None  = if nextMoveExists p pos lib then " +" else " ."
        char _ Black = " x"
        char _ White = " o"

-- some UI-related functions

transform :: (Move -> Move) -> Lib -> Lib
transform f l = l { moves = MoveSeq.transform f <| moves l }

mirror :: Lib -> Lib
mirror = transform (\ p -> Move.fromIntPartial (14 - Move.x p) (Move.y p))

rotate :: Lib -> Lib
rotate = transform (\ p -> Move.fromIntPartial (Move.y p) (14-Move.x p))