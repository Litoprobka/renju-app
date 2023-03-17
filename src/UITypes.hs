{-# LANGUAGE TemplateHaskell #-}

module UITypes where

import           DefaultImports

import           Lib            (Lib)
import           Move           (Move)
import           MoveSeq        (MoveSeq)
import           UndoRedoList   (UndoRedoList)

import qualified UndoRedoList   as URList

import           Monomer

data AppModel = AppModel {
  _libStates   :: UndoRedoList Lib,
  _editing     :: EditType,
  _readOnly    :: Bool,
  _currentFile :: Text
} deriving (Eq, Show)

-- | Configuration and runtime information
data Config = Config {
  _resources :: Text, -- ^ where app assets are stored
  _dataHome  :: Text   -- ^ XDG_DATA_HOME/renju-app, i.e. where lib files and screenshots are stored
}

type App a = Reader Config a

data AppEvent
  = LoadLib Text
  | LoadLibDialog
  | SaveLib Text
  | SaveLibDialog
  | SaveCurrentLib
  | NOOP
  | NewLib Lib
  | BoardClick Move Button Int
  | Rotate
  | Mirror
  | RemovePos
  | BoardText Move Text
  | SaveBoardText Move Text
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
  | ResetHistory
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

-- | Gets current lib state
lib :: Getting r AppModel Lib
lib = libStates . URList.current

isEditing :: Getting r AppModel Bool
isEditing = to (\model -> model ^. editing /= ENone)
