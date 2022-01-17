{-# LANGUAGE TemplateHaskell, ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}

module Lib where

import DefaultImports
import LitoUtils

import Move (Move)
import qualified Move

import MoveSeq (Stone(..), MoveSeq)
import qualified MoveSeq
import qualified Data.HashMap.Strict as HashMap
import Data.Default ( Default(..) )
import Data.Aeson
import Data.Text (snoc)
import Data.List.NonEmpty ((!!))

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
      _lib :: LibLayer
    , _moves :: MoveSeq
} deriving (Show, Eq)

instance ToJSON Lib where
    toJSON (Lib l _) = toJSON l

instance FromJSON Lib where
    parseJSON obj = do
        hmap <- parseJSON obj
        pure <| Lib hmap MoveSeq.empty

makeLenses 'Lib
makeLenses 'MoveInfo

-- | /O(1)/ An empty database
empty :: Lib
empty =
    Lib (one (MoveSeq.empty, def)) MoveSeq.empty

-- | O(1) A wrapper around MoveSeq.back
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
addPos mi ms = over lib (HashMap.insert ms mi)

-- | add a move to the lib
addMove :: Move -> Lib -> Lib
addMove move l =
    addPosDef pos <| set moves pos l
    where
        pos = MoveSeq.makeMove' move <| l^.moves

-- | checks if a given position exists in the Lib. If it does, switches to it
nextMove :: Move -> Lib -> Lib
nextMove move l =
    l |> applyIf (exists newPos) (moves .~ newPos)
    where
        newPos = l^.moves |> MoveSeq.makeMove' move

-- | removes a position from the lib
removePos :: MoveSeq -> Lib -> Lib
removePos ms
    | MoveSeq.isEmpty ms = const Lib.empty -- empty board can't be deleted
    | otherwise = over lib (HashMap.delete ms)

-- | removes a given position and all derivable positions from the lib
removeR :: MoveSeq -> Lib -> Lib
removeR pos =
    removePos pos
    .> applyAll (MoveSeq.mapNext (applyIf2 isOrphan removeR) pos)
    where
        isOrphan :: MoveSeq -> Lib -> Bool
        isOrphan pos' l' = exists pos' l' && (all (not <. flip exists l') <| MoveSeq.allPrev pos')
-- | removes current position from the lib
remove :: Lib -> Lib
remove l = l |> removeR (l^.moves) |> back

-- | apply a (MoveInfo -> MoveInfo) function to a given move in a lib
updatePos :: (MoveInfo -> MoveInfo) -> MoveSeq -> Lib -> Lib
updatePos f ms = over lib (HashMap.adjust f ms)

exists :: MoveSeq -> Lib -> Bool -- not sure about the argument order, maybe Move and Pos should be the other way around
exists ms = view lib .> HashMap.member ms

getPos :: MoveSeq -> Lib -> Maybe MoveInfo
getPos ms = view lib .> HashMap.lookup ms

currentPos :: Lib -> MoveInfo
currentPos (\l -> getPos (l^.moves) l -> Just mi) = mi
currentPos _ = error "current pos does not exist in the lib (impossible)"

-- manipulating comments and board text

-- | replace the comment for the current position
addComment :: Text -> Lib -> Lib
addComment t l = updatePos (set comment t) (l^.moves) l

-- | Returns the comment of the current position, "" if there is none
getComment :: Lib -> Text
getComment = currentPos .> view comment

-- Returns the comment of a given position, "" if there is none or the position does not exist in the lib
getCommentOf :: MoveSeq -> Lib -> Text
getCommentOf =
    getPos
    ..> map (view comment)
    ..> fromMaybe ""

getBoardText :: Move -> Lib -> Maybe Text
getBoardText move l = l
    |> currentPos
    |> view boardText
    |> HashMap.lookup newpos
    where
        newpos = l
            |> view moves
            |> MoveSeq.makeMove' move

-- | replace board text of a given move for the current position
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
        char ((`getBoardText` l) -> Just bt) Nothing =
            safeHead bt
            |> fromMaybe ' '
            |> snoc " "
        char move Nothing = if exists (MoveSeq.makeMove' move pos) l then " +" else " ."
        char _ (Just Black) = " x"
        char _ (Just White) = " o"

-- some UI-related functions

transform :: (Move -> Move) -> Lib -> Lib
transform f = moves %~ MoveSeq.transform f

mirror :: Lib -> Lib
mirror = transform <| Move.transformations !! 1

rotate :: Lib -> Lib
rotate = transform <| Move.transformations !! 6

fromText :: Text -> Maybe Lib
fromText =
    lines
    .> mapM MoveSeq.fromGetpos
    <.>> foldr' Lib.addPosDef Lib.empty
