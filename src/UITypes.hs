{-# LANGUAGE TemplateHaskell #-}

module UITypes where

import DefaultImports

import Lib (Lib)
import Move (Move)
import MoveSeq (MoveSeq, Stone (..))
import UndoRedoList (UndoRedoList)
import UndoRedoList qualified as URList

import Monomer hiding (black, white)

data AppModel = AppModel
  { _libStates :: UndoRedoList Lib
  , _editing :: EditType
  , _readOnly :: Bool
  , _currentFile :: Text
  }
  deriving (Eq, Show)

-- | Configuration and runtime information
data Config = Config
  { _dataHome :: Text
  -- ^ XDG_DATA_HOME/renju-app, i.e. where lib files and screenshots are stored
  , _stoneTextures :: StoneTypes Texture
  , -- I hope storing it as a function wouldn't hurt performance
    _boardTexture :: Texture
  }

data Texture = Texture ByteString Size

data StoneTypes a = StoneTypes
  { _blank :: a
  , _black :: a
  , _white :: a
  , _blackDot :: a
  , _whiteDot :: a
  }
  deriving (Eq, Show, Functor, Foldable, Traversable)

type App a = Reader Config a

data AppEvent
  = LoadLib Text
  | LoadLibDialog
  | SaveLib Text
  | SaveLibDialog
  | SaveCurrentLib
  | NOOP
  | LibChanging NonReadOnlyEvent
  | NewLib Text Lib
  | BoardClick Move Button Int
  | Rotate
  | Mirror
  | SavEditingBoardText Move Text
  | StopEditing
  | Comment Text
  | Getpos
  | Paste
  | Screenshot
  | ToggleReadOnly
  | ResetHistory
  deriving (Eq, Show)

data NonReadOnlyEvent -- these events should only do anything when the lib is not in read-only mode
  = RemovePos
  | BoardText Move Text
  | StartEditing Move
  | Putpos MoveSeq
  | Undo
  | Redo
  deriving (Eq, Show)

{-
RemovePos -> [updateLib Lib.remove]
    BoardText m t -> [updateLib <| Lib.addBoardText m t]
    StartEditing m ->
      [ editBoardText m (model ^. lib |> Lib.getBoardText m |> fromMaybe " ")
      , updateModel editing (const <| EditingBoardText m)
      ]
    Putpos ms -> [updateLib <| Lib.addPosRec ms]
    Undo -> [updateLibStates URList.undo]
    Redo
-}

data EditType
  = NotEditing
  | EditingBoardText Move
  deriving (Eq, Show)

type AppWenv = WidgetEnv AppModel AppEvent
type AppNode = WidgetNode AppModel AppEvent

makeLenses 'Config
makeLenses 'AppModel
makeLenses 'StoneTypes

-- normally I don't like the L postfix, but chances are 'stone' and 'dot'
-- would overlap with some variable names
stoneL :: Stone -> Getter (StoneTypes a) a
stoneL Black = black
stoneL White = white

dotL :: Stone -> Getter (StoneTypes a) a
dotL Black = blackDot
dotL White = whiteDot

-- | Gets the current lib state
lib :: Getting r AppModel Lib
lib = libStates . URList.current

isEditing :: Getting r AppModel Bool
isEditing = editing . to (/= NotEditing)
