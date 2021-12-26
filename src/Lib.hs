{-# LANGUAGE TemplateHaskell, ViewPatterns #-}

module Lib where

import DefaultImports
import LitoUtils

import Move (Move)
import qualified Move

import MoveSeq (Stone(..), MoveSeq)
import qualified MoveSeq

import qualified Data.RRBVector as Vec
import qualified Data.HashMap.Strict as HashMap
import Data.Default ( Default(..) )
import Data.Aeson
import qualified Data.Foldable as F (toList)

-- | Additional info for a position, such as board text and comments
data MoveInfo = MoveInfo
    { _comment :: Text
    , _boardText :: HashMap MoveSeq Text }
    deriving (Show, Eq)

instance Default MoveInfo where
    def = MoveInfo "" HashMap.empty

instance ToJSON MoveInfo where
    toJSON (MoveInfo c bt) = object ["comment" .= c, "board-text" .= bt]

instance FromJSON MoveInfo where
    parseJSON = withObject "MoveInfo" <| \obj -> MoveInfo 
            <$> obj .: "comment"
            <*> obj .: "board-text"

type LibLayer = HashMap MoveSeq MoveInfo
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

-- | Represents an empty database
empty :: Lib
empty =
    Lib (one (MoveSeq.empty, def) Vec.<| Vec.replicate 225 HashMap.empty) MoveSeq.empty

back :: Lib -> Lib
back = over moves MoveSeq.back

-- | add a position with default (blank) MoveInfo to the lib.
-- Does not overwrite the position if it already exists.
addPosDef :: MoveSeq -> Lib -> Lib
addPosDef m = applyIf (not <. exists m) (addPos def m)

-- | Add a position with given MoveInfo to the lib. 
-- Intended for loading a lib from a text file.
-- Overwrites MoveInfo if the position already exists
addPos :: MoveInfo -> MoveSeq -> Lib -> Lib
addPos mi = updateLibLayer (`HashMap.insert` mi)

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
removePos = updateLibLayer HashMap.delete

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
libLayerHelper f pos l = (l^.lib) Vec.! MoveSeq.moveCount pos |> f pos

-- | given a function that changes a libLayer based on a MoveSeq, apply it to correct LibLayer of a Lib
updateLibLayer :: (MoveSeq -> LibLayer -> LibLayer) -> MoveSeq -> Lib -> Lib
updateLibLayer f pos = lib %~ Vec.adjust (MoveSeq.moveCount pos) (f pos)

-- | apply a (MoveInfo -> MoveInfo) function to a given move in a lib
updatePos :: (MoveInfo -> MoveInfo) -> MoveSeq -> Lib -> Lib
updatePos f = updateLibLayer (HashMap.adjust f)

exists :: MoveSeq -> Lib -> Bool -- not sure about the argument order, maybe Move and Pos should be the other way around
exists = libLayerHelper HashMap.member

getPos :: MoveSeq -> Lib -> Maybe MoveInfo
getPos = libLayerHelper HashMap.lookup

currentPos :: Lib -> MoveInfo
currentPos (\l -> getPos (l^.moves) l -> Just mi) = mi
currentPos _ = error "current pos does not exist in the lib (impossible)"

-- manipulating comments and board text

-- | replace the comment for the current position
addComment :: Text -> Lib -> Lib
addComment t l = updatePos (set comment t) (l^.moves) l

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
            |> view moves
            |> MoveSeq.makeMove' move

-- | replcae board text of a given move for the current position
addBoardText :: Move -> Text -> Lib -> Lib
addBoardText m t l = updatePos (over boardText upd) pos l where
    btpos = MoveSeq.makeMove' m pos
    upd = if btpos /= pos then HashMap.insert btpos t else id
    pos = l^.moves

printLib :: Lib -> IO ()
printLib l =
    pos
    |> MoveSeq.toText char
    |> (<> ("Comment: " <> getComment l <> "\n"))
    |> putText
    where
        pos = l^.moves

        --char move None = case MoveSeq.makeMove' move pos of
        char ((`getBoardText` l) -> Just bt) None = 
            safeHead bt
            |> fromMaybe ' '
            |> (one :: Char -> Text)
            |> (" " <>)
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

fromText :: Text -> Maybe Lib
fromText =
    lines
    .> mapM MoveSeq.fromGetpos
    <.>> foldr' Lib.addPosDef Lib.empty
