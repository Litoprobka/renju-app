module Lib where

import Universum
import Flow

import Point (Point)
import qualified Point

import Pos (Pos, Move(..), moveCount)
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
    , moves :: [Point]
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
addMove :: Point -> Lib -> Lib
addMove point l@(Lib lib moves)
    | point `elem` moves = l -- if the current position already has this move, do nothing
    | otherwise = addPos pos l { moves = newMoves }
    where
        newMoves = point : moves
        pos = Pos.fromPointList newMoves

-- | removes a position from the lib
removePos :: Pos -> Lib -> Lib
removePos pos l =
    l { lib = lib l |> Seq.adjust (HashMap.delete pos) (moveCount pos) }

-- | removes current position from the lib
remove :: Lib -> Lib
remove l@(Lib _ []) = l
remove l = l |> removePos (Pos.fromPointList <| moves l) |> back

nextMoveHelper :: (Pos -> LibLayer -> a) -> Point -> Pos -> Lib -> a
nextMoveHelper f point pos l = lib l `Seq.index` Pos.moveCount newPos |> f newPos where
    newPos = Pos.makeMove' point pos 

nextMoveExists :: Point -> Pos -> Lib -> Bool -- not sure about the argument order, maybe Point and Pos should be the other way around
nextMoveExists = nextMoveHelper HashMap.member

getNextMove :: Point -> Pos -> Lib -> Maybe MoveInfo
getNextMove = nextMoveHelper HashMap.lookup

printLib :: Lib -> IO ()
printLib lib =
    pos
    |> Pos.toText char
    |> putText
    where
        pos = Pos.fromPointList <| moves lib

        char p None  = if nextMoveExists p pos lib then " +" else " ."
        char _ Black = " x"
        char _ White = " o"

-- some UI-related functions

transform :: (Point -> Point) -> Lib -> Lib
transform f l = l { moves = moves l <&> f }

mirror :: Lib -> Lib
mirror = transform (\ p -> Point.fromIntPartial (14 - Point.x p) (Point.y p))

rotate :: Lib -> Lib
rotate = transform (\ p -> Point.fromIntPartial (Point.y p) (14-Point.x p))