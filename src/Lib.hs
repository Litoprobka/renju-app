{-# LANGUAGE TemplateHaskell #-}

module Lib where

import Universum hiding (over, view, (^.), (%~), set) -- Universum re-exports microlens (but not microlens-th), while Monomer depends on lens. Ewww...
import Control.Lens hiding ((.>), (<.), (<|), (|>), transform)
import Flow

import Move (Move)
import qualified Move

import Pos (Stone(..))
import LitoUtils

import MoveSeq (MoveSeq)
import qualified MoveSeq

import qualified Data.Sequence as Seq
import qualified Data.HashMap.Strict as HashMap

-- | Additional info for a position, such as comments
newtype MoveInfo = MoveInfo {
    comment :: Text
    -- other stuff like board text, previous move (if needed)
} deriving (Show, Eq)

-- instance Default MoveInfo where 
defMoveInfo :: MoveInfo
defMoveInfo = MoveInfo ""

type LibLayer = HashMap MoveSeq MoveInfo
-- | Represents a database file
data Lib = Lib {
      _lib :: Seq LibLayer
    , _moves :: MoveSeq
} deriving (Show, Eq)

makeLenses 'Lib

-- | Represents an empty database
empty :: Lib
empty =
    Lib (Seq.replicate 225 HashMap.empty) MoveSeq.empty

back :: Lib -> Lib
back = over moves MoveSeq.back

-- | add a position to the lib
addPos :: MoveSeq -> Lib -> Lib
addPos pos l
    | exists pos l = l
    | otherwise =
        l |> lib %~ Seq.adjust (HashMap.insert pos defMoveInfo) (MoveSeq.moveCount pos)

-- | add a move to the lib
addMove :: Move -> Lib -> Lib
addMove move l =
    addPos pos <| set moves pos l
    where
        pos = MoveSeq.makeMove' move <| l^.moves

-- | removes a position from the lib
removePos :: MoveSeq -> Lib -> Lib
removePos pos l
    | MoveSeq.isEmpty pos = l
    | otherwise = l |> lib %~ Seq.adjust (HashMap.delete pos) (MoveSeq.moveCount pos)

-- | removes a position and all derivable positions from the lib
removeR :: MoveSeq -> Lib -> Lib
removeR pos =
        removePos pos
        .> applyAll (MoveSeq.mapNext (applyIf2 isOrphan removeR) pos)
        where
            isOrphan :: MoveSeq -> Lib -> Bool -- wip
            isOrphan pos' l' = exists pos' l' && (all (not <. flip exists l') <| MoveSeq.allPrev pos')
-- | removes current position from the lib
remove :: Lib -> Lib
remove l = l |> removePos (l^.moves) |> back

-- | given a function that takes a MoveSeq and LibLayer, apply it to correct LibLayer of a Lib
libLayerHelper :: (MoveSeq -> LibLayer -> a) -> MoveSeq -> Lib -> a
libLayerHelper f pos l = (l^.lib) `Seq.index` MoveSeq.moveCount pos |> f pos

-- | given a functions that changes a libLayer based on a MoveSeq, appyl it to correct LibLayer of a Lib
updateLibLayer :: (MoveSeq -> LibLayer -> LibLayer) -> MoveSeq -> Lib -> Lib
updateLibLayer f pos = lib %~ Seq.adjust (f pos) (MoveSeq.moveCount pos)

exists :: MoveSeq -> Lib -> Bool -- not sure about the argument order, maybe Move and Pos should be the other way around
exists = libLayerHelper HashMap.member

getPos :: MoveSeq -> Lib -> Maybe MoveInfo
getPos = libLayerHelper HashMap.lookup

printLib :: Lib -> IO ()
printLib l =
    pos
    |> MoveSeq.toText char
    |> putText
    where
        pos = l^.moves

        char p None  = if exists (MoveSeq.makeMove' p pos) l then " +" else " ."
        char _ Black = " x"
        char _ White = " o"

-- some UI-related functions

transform :: (Move -> Move) -> Lib -> Lib
transform f = moves %~ MoveSeq.transform f

mirror :: Lib -> Lib
mirror = transform (\ p -> Move.fromIntPartial (14 - Move.getX p) (Move.getY p))

rotate :: Lib -> Lib
rotate = transform (\ p -> Move.fromIntPartial (Move.getY p) (14-Move.getX p))