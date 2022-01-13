{-# LANGUAGE TemplateHaskell, ViewPatterns #-}

module Lib where

import DefaultImports
import LitoUtils

import Move (Move)
import qualified Move

import MoveSet (Stone(..), MoveSet)
import MoveSeq (MoveSeq, toSet)
import qualified MoveSeq
import qualified MoveSet

import qualified Data.RRBVector as Vec
import qualified Data.HashMap.Strict as HashMap
import Data.Default ( Default(..) )
import Data.Aeson
import qualified Data.Foldable as F (toList)
import Data.Text (snoc)
import Lens.Micro(SimpleGetter, to)

-- | Additional info for a position, such as board text and comments
data MoveInfo = MoveInfo
    { _comment :: Text
    , _boardText :: HashMap MoveSet Text }
    deriving (Show, Eq)

instance Default MoveInfo where
    def = MoveInfo "" HashMap.empty

instance ToJSON MoveInfo where
    toJSON (MoveInfo c bt) = object ["comment" .= c, "board-text" .= bt]

instance FromJSON MoveInfo where
    parseJSON = withObject "MoveInfo" <| \obj -> MoveInfo
            <$> obj .: "comment"
            <*> obj .: "board-text"

type LibLayer = HashMap MoveSet MoveInfo
-- | Represents a database file
data Lib = Lib {
      _lib :: Vec.Vector LibLayer
    , _moves :: MoveSeq
} deriving (Show, Eq)

instance ToJSON Lib where
    toJSON (Lib l _) = toJSON <| F.toList l -- RRB Vector does not play well with Universum

instance FromJSON Lib where
    parseJSON obj = do
        libLayers <- Vec.fromList <$> parseJSON obj
        pure <| Lib libLayers MoveSeq.empty

makeLenses 'Lib
makeLenses 'MoveInfo

moveSet :: SimpleGetter Lib MoveSet
moveSet = to <| view moves .> MoveSeq.toSet

-- | Represents an empty database
empty :: Lib
empty =
    Lib (one (MoveSet.empty, def) Vec.<| Vec.replicate 225 HashMap.empty) MoveSeq.empty

back :: Lib -> Lib
back = over moves MoveSeq.back

-- | add a position with default (blank) MoveInfo to the lib.
-- Does not overwrite the position if it already exists.
addPosDef :: MoveSet -> Lib -> Lib
addPosDef m = applyIf (not <. exists m) (addPos def m)

-- | Add a position with given MoveInfo to the lib. 
-- Intended for loading a lib from a text file.
-- Overwrites MoveInfo if the position already exists
addPos :: MoveInfo -> MoveSet -> Lib -> Lib
addPos mi = updateLibLayer (`HashMap.insert` mi)

-- | add a move to the lib
addMove :: Move -> Lib -> Lib
addMove move l =
    addPosDef (toSet pos) <| set moves pos l
    where
        pos = MoveSeq.makeMove' move <| l^.moves

-- | checks if a child position with the given move exists in the Lib. If it does, switch to it
nextMove :: Move -> Lib -> Lib
nextMove move l =
    l |> applyIf (exists <| toSet newPos) (moves .~ newPos)
    where
        newPos = l^.moves |> MoveSeq.makeMove' move

-- | removes a position from the lib
removePos :: MoveSet -> Lib -> Lib
removePos ms
    | MoveSet.isEmpty ms = const Lib.empty -- empty board can't be deleted
    | otherwise = ms |> updateLibLayer HashMap.delete

-- | removes a given position and all derivable positions from the lib
removeR :: MoveSet -> Lib -> Lib
removeR pos =
    removePos pos
    .> applyAll (MoveSet.mapNext (applyIf2 isOrphan removeR) pos)
    where
        isOrphan :: MoveSet -> Lib -> Bool
        isOrphan pos' l' = exists pos' l' && (all (not <. flip exists l') <| MoveSet.allPrev pos')
-- | removes current position from the lib
remove :: Lib -> Lib
remove l = l |> removeR (l ^. moveSet) |> back

-- | given a function that takes a MoveSet and LibLayer, apply it to correct LibLayer of a Lib
libLayerHelper :: (MoveSet -> LibLayer -> a) -> MoveSet -> Lib -> a
libLayerHelper f pos l = (l^.lib) Vec.! MoveSet.moveCount pos |> f pos

-- | given a function that changes a libLayer based on a MoveSeq, apply it to correct LibLayer of a Lib
updateLibLayer :: (MoveSet -> LibLayer -> LibLayer) -> MoveSet -> Lib -> Lib
updateLibLayer f pos = lib %~ Vec.adjust (MoveSet.moveCount pos) (f pos)

-- | apply a (MoveInfo -> MoveInfo) function to a given move in a lib
updatePos :: (MoveInfo -> MoveInfo) -> MoveSet -> Lib -> Lib
updatePos f = updateLibLayer (HashMap.adjust f)

exists :: MoveSet -> Lib -> Bool -- not sure about the argument order, maybe Move and Pos should be the other way around
exists = libLayerHelper HashMap.member

getPos :: MoveSet -> Lib -> Maybe MoveInfo
getPos = libLayerHelper HashMap.lookup

currentPos :: Lib -> MoveInfo
currentPos (\l -> getPos (l^.moveSet) l -> Just mi) = mi
currentPos _ = error "current pos does not exist in the lib (impossible)"

-- manipulating comments and board text

-- | replace the comment for the current position
addComment :: Text -> Lib -> Lib
addComment t l = updatePos (set comment t) (l^.moveSet) l

-- | returns the comment of the current position
getComment :: Lib -> Text
getComment = currentPos .> view comment

getBoardText :: Move -> Lib -> Maybe Text
getBoardText move l = l
    |> currentPos
    |> view boardText
    |> HashMap.lookup newpos
    where
        newpos = l
            |> view moveSet
            |> MoveSet.makeMove' move

-- | replcae board text of a given move for the current position
addBoardText :: Move -> Text -> Lib -> Lib
addBoardText m t l = updatePos (over boardText upd) pos l where
    btpos = MoveSet.makeMove' m pos
    upd = if btpos /= pos then HashMap.insert btpos t else id
    pos = l^.moveSet

printLib :: Lib -> IO ()
printLib l =
    pos
    |> MoveSet.toText char
    |> (<> ("Comment: " <> getComment l <> "\n"))
    |> putText
    where
        pos = l^.moveSet

        --char move None = case MoveSeq.makeMove' move pos of
        char ((`getBoardText` l) -> Just bt) Nothing =
            safeHead bt
            |> fromMaybe ' '
            |> snoc " "
        char move Nothing = if exists (MoveSet.makeMove' move pos) l then " +" else " ."
        char _ (Just Black) = " x"
        char _ (Just White) = " o"

-- some UI-related functions

transform :: (Move -> Move) -> Lib -> Lib
transform f = moves %~ MoveSeq.transform f

mirror :: Lib -> Lib
mirror = transform (\ p -> Move.fromIntPartial (14 - Move.getX p) (Move.getY p))

rotate :: Lib -> Lib
rotate = transform (\ p -> Move.fromIntPartial (Move.getY p) (14-Move.getX p))

fromText :: Text -> Maybe Lib
fromText =
    lines
    .> mapM MoveSet.fromGetpos
    <.>> foldr' Lib.addPosDef Lib.empty
