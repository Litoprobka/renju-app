{-# LANGUAGE TemplateHaskell, ViewPatterns #-}

module MoveSeq where

import DefaultImports
import LitoUtils
import Move(Move)
import qualified MoveSet as MS

data MoveSeq = MoveSeq {
    _moveList :: [Move],
    _moveSet :: MS.MoveSet
} deriving Show

makeLenses 'MoveSeq

instance Eq MoveSeq where
    (==) = (==) `on` view moveSet

toSet :: MoveSeq -> MS.MoveSet
toSet = view moveSet

empty :: MoveSeq
empty = MoveSeq [] MS.empty

isEmpty :: MoveSeq -> Bool
isEmpty = view moveList .> null

back :: MoveSeq -> MoveSeq
back (MoveSeq [] _) = MoveSeq.empty
back (MoveSeq (m : mlist) mset) =
    MoveSeq mlist (mset |> MS.remove m)

makeMove :: Move -> MoveSeq -> Maybe MoveSeq
makeMove m ms = do
    mset' <- MS.makeMove m (ms ^. moveSet)
    pure <| MoveSeq (m : ms ^. moveList) mset'

makeMove' :: Move -> MoveSeq -> MoveSeq
makeMove' m ms = fromMaybe ms (makeMove m ms)

transform :: (Move -> Move) -> MoveSeq -> MoveSeq
transform f =
       over moveList (map f)
    .> over moveSet (MS.transform f)