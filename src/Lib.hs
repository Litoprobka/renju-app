module Lib where

import Universum
import Flow

import Point (Point)
import qualified Point

import Pos (Pos, Move(..), moveCount)
import qualified Pos

import qualified Data.Sequence as Seq
import Data.Map.Strict (Map, (!?))
import qualified Data.Map.Strict as Map -- should I use a HashMap instead?
import Data.Text

-- | Additional info for a position, such as comments
data MoveInfo = MoveInfo {
    comment :: Text
    -- other stuff like board text, previous move (if needed)
}

-- instance Default MoveInfo where 
defMoveInfo = MoveInfo ""

type LibLayer = Map Pos MoveInfo
type UnwrappedLib = Seq LibLayer
-- | Represents a database file
newtype Lib = Lib UnwrappedLib -- again, I need a static-length array type

-- | Represents an empty database
empty :: Lib
empty =
    Seq.replicate 225 Map.empty
    |> Lib

-- | An fmap-like function that unwraps a lib and wraps it back, not exported
fmapLib :: (UnwrappedLib -> UnwrappedLib) -> (Lib -> Lib)
fmapLib f (Lib l) =
    f l |> Lib

-- | add a position to the lib
add :: Pos -> Lib -> Lib
add pos =
    Seq.adjust (Map.insert pos defMoveInfo) (moveCount pos) -- something screams that I need a container with fast random access
    |> fmapLib

-- | removes a position from the lib
remove :: Pos -> Lib -> Lib
remove pos =
    Seq.adjust (Map.delete pos) (moveCount pos)
    |> fmapLib

nextMoveHelper :: (Pos -> LibLayer -> a) -> Point -> Pos -> Lib -> a
nextMoveHelper f point pos (Lib lib) = lib `Seq.index` Pos.moveCount newPos |> f newPos where
    newPos = Pos.makeMove' point pos 

nextMoveExists :: Point -> Pos -> Lib -> Bool -- not sure about the argument order, maybe Point and Pos should be the other way around
nextMoveExists = nextMoveHelper Map.member

getNextMove :: Point -> Pos -> Lib -> Maybe MoveInfo
getNextMove = nextMoveHelper Map.lookup

printLibAt :: Pos -> Lib -> IO ()
printLibAt pos lib =
    pos
    |> Pos.toText char
    |> putText
    where
        char p None  = if nextMoveExists p pos lib then " +" else " ."
        char _ Black = " x"
        char _ White = " o"