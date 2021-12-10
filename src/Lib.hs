{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Lib where

import Universum hiding (over, view, (^.), (%~), set, (.~)) -- Universum re-exports microlens (but not microlens-th), while Monomer depends on lens. Ewww...
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
import qualified Data.Text as Text (concat)
import Data.Default ( Default(..) )

-- | Additional info for a position, such as board text and comments
data MoveInfo = MoveInfo 
    { _comment :: Text
    , _boardText :: HashMap Move Text }
    deriving (Show, Eq)

instance Default MoveInfo where 
    def = MoveInfo "" HashMap.empty

type LibLayer = HashMap MoveSeq MoveInfo
-- | Represents a database file
data Lib = Lib {
      _lib :: Seq LibLayer
    , _moves :: MoveSeq
} deriving (Show, Eq)

makeLenses 'Lib
makeLenses 'MoveInfo

-- | Represents an empty database
empty :: Lib
empty =
    Lib (one (MoveSeq.empty, def) Seq.<| Seq.replicate 225 HashMap.empty) MoveSeq.empty

back :: Lib -> Lib
back = over moves MoveSeq.back

-- | add a position with default (blank) MoveInfo to the lib
addPosDef :: MoveSeq -> Lib -> Lib
addPosDef = addPos def
    
-- | add a position with given MoveInfo to the lib. Used when loading a lib from a text file.
addPos :: MoveInfo -> MoveSeq -> Lib -> Lib
addPos mi pos l
    | exists pos l = l
    | otherwise =
        l |> lib %~ Seq.adjust (HashMap.insert pos mi) (MoveSeq.moveCount pos)

-- | add a move to the lib
addMove :: Move -> Lib -> Lib
addMove move l =
    addPosDef pos <| set moves pos l
    where
        pos = MoveSeq.makeMove' move <| l^.moves

-- | checks if a given position exists in the Lib. If it does, switch to it
nextMove :: Move -> Lib -> Lib
nextMove move l =
    l |> applyIf (exists newPos) (moves .~ newPos)
    where
        newPos = l^.moves |> MoveSeq.makeMove' move

-- | removes a position from the lib
removePos :: MoveSeq -> Lib -> Lib
removePos pos =
    lib %~ Seq.adjust (HashMap.delete pos) (MoveSeq.moveCount pos)

-- | removes a given position and all derivable positions from the lib
removeR :: MoveSeq -> Lib -> Lib
removeR pos
    | MoveSeq.isEmpty pos = const Lib.empty -- MoveSeq.allPrev does not work properly for positions with one move
    | otherwise =
        removePos pos
        .> applyAll (MoveSeq.mapNext (applyIf2 isOrphan removeR) pos)
        where
            isOrphan :: MoveSeq -> Lib -> Bool
            isOrphan pos' l' = exists pos' l' && (all (not <. flip exists l') <| MoveSeq.allPrev pos')
-- | removes current position from the lib
remove :: Lib -> Lib
remove l = l |> removeR (l^.moves) |> back

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

currentPos :: Lib -> MoveInfo
currentPos (\l -> getPos (l^.moves) l -> Just mi) = mi
currentPos _ = error "current pos does not exist in the lib (impossible)"

printLib :: Lib -> IO ()
printLib l =
    pos
    |> MoveSeq.toText char
    |> putText
    where
        pos = l^.moves

        char (currentPos l |> view boardText |> (HashMap.!?) -> Just bt) None = bt
        char move None = if exists (MoveSeq.makeMove' move pos) l then " +" else " ."
        char _ Black = " x"
        char _ White = " o"

-- some UI-related functions

transform :: (Move -> Move) -> Lib -> Lib
transform f = moves %~ MoveSeq.transform f

mirror :: Lib -> Lib
mirror = transform (\ p -> Move.fromIntPartial (14 - Move.getX p) (Move.getY p))

rotate :: Lib -> Lib
rotate = transform (\ p -> Move.fromIntPartial (Move.getY p) (14-Move.getX p))

-- storing Lib as readable text
-- does not store MoveInfo yet
toText :: Lib -> Text
toText =
    view lib
    .> Seq.filter (not <. null)
    <.>> (
        keys
        <.>> MoveSeq.toGetpos
        .> unlines
        )
    .> toList
    .> Text.concat

fromText :: Text -> Maybe Lib
fromText =
    lines
    .> mapM MoveSeq.fromGetpos
    <.>> foldr' Lib.addPosDef Lib.empty
