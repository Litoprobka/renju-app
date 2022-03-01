{-# LANGUAGE TemplateHaskell #-}

module UITypes where

import DefaultImports

import Move (Move)
import MoveSeq (MoveSeq)
import UndoRedoList (UndoRedoList)
import Lib (Lib)

import qualified UndoRedoList as URList

import Monomer

data AppModel = AppModel {
  _libStates :: UndoRedoList Lib,
  _editing :: EditType
} deriving (Eq, Show)

data AppEvent
  = LoadDefaultLib
  | SaveDefaultLib
  | Blank
  | NewLib Lib
  | BoardClick Move Button Int
  | Rotate
  | Mirror
  | RemovePos
  | BoardText Move Text
  | StartEditing Move
  | StopEditing
  | Comment Text
  | Getpos
  | Paste
  | Putpos MoveSeq
  | Undo
  | Redo
  | Screenshot
  deriving (Eq, Show)

data EditType
  = ENone
  | EBoardText Move
  deriving (Eq, Show)

type AppWenv = WidgetEnv AppModel AppEvent
type AppNode = WidgetNode AppModel AppEvent

makeLenses 'AppModel

data BTEvent
  = Save
  | Cancel
  deriving (Show, Eq)

data BTModel = BTModel {
  _move :: Move,
  _boardText :: Text
} deriving (Show, Eq)

type BTWenv = WidgetEnv BTModel BTEvent
type BTNode = WidgetNode BTModel BTEvent

makeLenses 'BTModel

-- | Gets current lib state
lib :: Getting r AppModel Lib
lib = libStates . URList.current

isEditing :: Getting r AppModel Bool
isEditing = to (\model -> model ^. editing /= ENone)