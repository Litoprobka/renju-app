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
  _editing :: EditType,
  _readOnly :: Bool
} deriving (Eq, Show)

-- | Configuration and runtime information
data Config = Config {
  _resources :: Text, -- ^ where app assets are stored
  _dataHome :: Text   -- ^ XDG_DATA_HOME/renju-app, i.e. where lib files and screenshots are stored
}

type App a = Reader Config a

data AppEvent
  = LoadDefaultLib
  | SaveDefaultLib
  | NOOP
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
  | ToggleReadOnly
  deriving (Eq, Show)

data EditType
  = ENone
  | EBoardText Move
  deriving (Eq, Show)

type AppWenv = WidgetEnv AppModel AppEvent
type AppNode = WidgetNode AppModel AppEvent

makeLenses 'Config
makeLenses 'AppModel

-- not sure if this function belongs in UITypes
getResourcesDir :: App Text
getResourcesDir =
  ask <&> view resources


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