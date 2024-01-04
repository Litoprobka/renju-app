{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module UndoRedoList where

import DefaultImports

data UndoRedoList a = UndoRedoList
  { -- I might be reinventing the wheel here, but it seems simple enough to implement
    _prev :: [a]
  , _current :: a
  , _next :: [a]
  }
  deriving (Eq, Show)

makeLenses 'UndoRedoList

-- | Returns a URList with given state and no undo/redo history
one :: a -> UndoRedoList a
one x = UndoRedoList [] x []

undo :: UndoRedoList a -> UndoRedoList a
undo l@UndoRedoList{..} = case _prev of
  [] -> l
  prevHead : prevTail ->
    UndoRedoList prevTail prevHead (_current : _next)

redo :: UndoRedoList a -> UndoRedoList a
redo l@UndoRedoList{..} = case _next of
  [] -> l
  nextHead : nextTail ->
    UndoRedoList (_current : _prev) nextHead nextTail

update :: (a -> a) -> UndoRedoList a -> UndoRedoList a
update f UndoRedoList{..} = UndoRedoList (_current : _prev) (f _current) []

add :: a -> UndoRedoList a -> UndoRedoList a
add x = update (const x)
