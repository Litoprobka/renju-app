{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Lib where

import DefaultImports

import Move (Move)
import Move qualified
import MoveSeq (MoveSeq)
import MoveSeq qualified

import Control.Exception (PatternMatchFail (..))
import Control.Lens (lens)
import Data.Aeson
import Data.Default (Default (..))
import Data.HashMap.Strict qualified as HashMap
import Data.Text qualified as Text (length)
import PosID (PosID, fromMoveSeq, toMoveSeq)

-- | Additional info for a position, such as board text and comments
data MoveInfo = MoveInfo
  { _comment :: Text
  , _boardText :: HashMap PosID Text
  }
  deriving (Show, Eq)

-- | Represents a database file
data Lib = Lib
  { _lib :: HashMap PosID MoveInfo
  , _moves :: MoveSeq
  }
  deriving (Show, Eq)

-- * Instances
instance Default MoveInfo where
  def = MoveInfo "" HashMap.empty

instance ToJSON MoveInfo where
  toJSON (MoveInfo c bt) = object ["comment" .= c, "board-text" .= bt]

instance FromJSON MoveInfo where
  parseJSON =
    withObject "MoveInfo" <| \obj ->
      MoveInfo
        <$> obj
        .: "comment"
        <*> obj
        .: "board-text"
instance ToJSON Lib where
  toJSON (Lib l _) = toJSON l

instance FromJSON Lib where
  parseJSON obj = do
    hmap <- parseJSON obj
    pure <| Lib hmap MoveSeq.empty

makeLenses 'Lib
makeLenses 'MoveInfo

-- * Adding / removing positions

-- | /O(1)./ An empty database
empty :: Lib
empty =
  Lib (one (PosID.fromMoveSeq MoveSeq.empty, def)) MoveSeq.empty

-- | /O(1)./ A wrapper around MoveSeq.back
back :: Lib -> Lib
back = moves %~ MoveSeq.back

toMove :: Move -> Lib -> Lib
toMove move = moves %~ MoveSeq.toMove move

{- | /O(log n)./ Add a position with default (blank) MoveInfo to the lib.
Does not overwrite the position if it already exists.
-}
addPosDef :: MoveSeq -> Lib -> Lib
addPosDef = applyIf2 (not <.. exists) (addPos def)

{- | /O(log n)./ Add a position with given MoveInfo to the lib.
Intended for loading a lib from a text file.
Overwrites MoveInfo if the position already exists
-}
addPos :: MoveInfo -> MoveSeq -> Lib -> Lib
addPos moveInfo moveSeq = pos moveSeq ?~ moveInfo

-- | /O(log(n) * m)./ Add a positition to the lib move by move
addPosRec :: MoveSeq -> Lib -> Lib
addPosRec pos l =
  flipfoldl' addMove (l |> moves .~ MoveSeq.empty) (MoveSeq.moveList pos)

-- | /O(log n)./ Add a move to the lib
addMove :: Move -> Lib -> Lib
addMove move l =
  addPosDef pos <| set moves pos l
 where
  pos = MoveSeq.makeMove' move <| l ^. moves

-- | /O(log n)./ Check if a given position exists in the Lib. If it does, switch to it
nextMove :: Move -> Lib -> Lib
nextMove move l =
  l |> applyIf (exists newPos) (moves .~ newPos)
 where
  newPos = l ^. moves |> MoveSeq.makeMove' move

-- | /O(log n)./ Remove a position from the lib
removePos :: MoveSeq -> Lib -> Lib
removePos moveSeq
  | MoveSeq.isEmpty moveSeq = const Lib.empty -- empty board can't be deleted
  | otherwise = pos moveSeq .~ Empty

-- | Remove the given position and all of its derivable positions from the lib
removeR :: MoveSeq -> Lib -> Lib
removeR pos =
  removePos pos
    .> applyAll (MoveSeq.mapNext (applyIf2 isOrphan removeR) pos)
 where
  isOrphan :: MoveSeq -> Lib -> Bool
  isOrphan pos' l' = exists pos' l' && (all (not <. flip exists l') <| MoveSeq.allPrev pos')

-- | Remove current position from the lib
remove :: Lib -> Lib
remove l = l |> removeR (l ^. moves) .> back

-- | Merge two libs, prioritising positions, comments and board text from the first one
merge :: Lib -> Lib -> Lib
merge l1 l2 = lib %~ HashMap.unionWith mergeMoveInfo (l2 ^. lib) <| l1
 where
  mergeMoveInfo m1 m2 =
    MoveInfo (maxBy Text.length (m1 ^. comment) (m2 ^. comment)) <|
      HashMap.unionWith (maxBy Text.length) (m1 ^. boardText) (m2 ^. boardText)

-- * Adding / updating comments and board text

-- | /O(log n)./ Apply a (MoveInfo -> MoveInfo) function to a given move in a lib
updatePos :: (MoveInfo -> MoveInfo) -> MoveSeq -> Lib -> Lib
updatePos f moveSeq = pos' moveSeq %~ f

-- | /O(log n)./ Replace the comment for the current position
addComment :: Text -> Lib -> Lib
addComment t l = updatePos (set comment t) (l ^. moves) l

-- | /O(log n)./ Replace board text of a given move for the current position
addBoardText :: Move -> Text -> Lib -> Lib
addBoardText m t l = l |> currentPos . boardText . at' btpos . non "" .~ t
 where
  btpos = MoveSeq.makeMove' m <| l ^. moves

-- * Lens

-- | /O(log n)./ Focuses MoveInfo of a given position, or lack thereof
pos :: MoveSeq -> Lens' Lib (Maybe MoveInfo)
pos moveSeq = lib . at' moveSeq

-- | /O(log n)./ Focuses MoveInfo of a given position
pos' :: MoveSeq -> Traversal' Lib MoveInfo
pos' moveSeq = lib . ix (PosID.fromMoveSeq moveSeq)

-- | /O(log n)./ Focuses MoveInfo of the current position
currentPos :: Lens' Lib MoveInfo
currentPos = lens getPos setPos
 where
  getPos l =
    l ^. pos (l ^. moves)
      |> fromMaybe
        ( bug <. PatternMatchFail <|
            toString (MoveSeq.toGetpos (l ^. moves)) <> " does not exist in the lib"
        )

  setPos l moveInfo = l |> addPos moveInfo (l ^. moves)

-- | /O(log n)./ Focuses the comment of the current position, "" if there is none
comment' :: Lens' Lib Text
comment' = currentPos . comment

-- | A wrapper around `at` that treats MoveSeq as PosID
at' :: MoveSeq -> Lens' (HashMap PosID a) (Maybe a)
at' moveSeq = at (PosID.fromMoveSeq moveSeq)

-- * Queries

isEmpty :: Lib -> Bool
isEmpty = (== Lib.empty)

-- | /O(log n)./ Check if a given position exists in the lib
exists :: MoveSeq -> Lib -> Bool -- not sure about the argument order, maybe Move and Pos should be the other way around
exists moveSeq = has (pos' moveSeq)

-- | /O(log n)./ Return the comment of a given position, "" if there is none or the position does not exist
getCommentOf :: MoveSeq -> Lib -> Text
getCommentOf moveSeq =
  view (pos' moveSeq . comment)
{- ^ seems like `view` silently falls back on Monoid instance of Text
luckily, that's exactly what we want here
-}

-- | /O(log n * log m)./ Get current pos' board text for a given point on the board
getBoardText :: Move -> Lib -> Maybe Text
getBoardText move l = do
  newPos <- MoveSeq.makeMove move <| l ^. moves
  l ^. currentPos . boardText . at' newPos

-- * Some UI-related functions

transform :: (Move -> Move) -> Lib -> Lib
transform f = moves %~ MoveSeq.transform f

mirror :: Lib -> Lib
mirror = transform Move.transformations.invertX

rotate :: Lib -> Lib
rotate = transform Move.transformations.rotateR -- swapInvertY

repair :: Lib -> Lib
repair l = HashMap.foldlWithKey' go l (l ^. lib)
 where
  go accLib pos _ = addPosRec (PosID.toMoveSeq pos) accLib
