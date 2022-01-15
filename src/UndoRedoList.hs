{-# LANGUAGE TemplateHaskell, ViewPatterns #-}

module UndoRedoList where

import DefaultImports

data UndoRedoList a = UndoRedoList { -- I might be reinventing the wheel here, but it seems simple enough to implement
    _prev :: [a],
    _current :: a,
    _next :: [a]
} deriving (Eq, Show)

makeLenses 'UndoRedoList

-- | Returns a URList with given state and no undo/redo history
one :: a -> UndoRedoList a
one x = UndoRedoList [] x []

undo :: UndoRedoList a -> UndoRedoList a
undo l@(UndoRedoList pr cur nxt) = case pr of
    [] -> l
    prevHead : prevTail ->
        UndoRedoList prevTail prevHead (cur : nxt)

redo :: UndoRedoList a -> UndoRedoList a
redo l@(UndoRedoList pr cur nxt) = case nxt of
  [] -> l
  nextHead : nextTail ->
      UndoRedoList (cur : pr) nextHead nextTail

update :: (a -> a) -> UndoRedoList a -> UndoRedoList a
update f (UndoRedoList pr cur _) = UndoRedoList (cur : pr) (f cur) []

add :: a -> UndoRedoList a -> UndoRedoList a
add x = update (const x)
