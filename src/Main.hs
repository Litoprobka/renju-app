{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import DefaultImports

import EventHandling (handleEvent)
import IO (pickSubcommand)
import Lib qualified
import TH (embedImage, embedResource)
import UI (buildUI, textureNames)
import UITypes
import UndoRedoList qualified as URList

import Monomer

import System.Directory (XdgDirectory (XdgData), createDirectoryIfMissing, getXdgDirectory)

main :: IO ()
main = do
  _dataHome <-
    lookupEnv "RENJU_APP_DIR"
      & onNothingM (getXdgDirectory XdgData "renju-app")
      <&> fromString

  createDirectoryIfMissing True (toString _dataHome) -- create $RENJU_APP_DIR / $XDG_DATA_HOME/renju-app
  let appCfg = Config{..}
      libPathOrDef = fromMaybe (_dataHome <> "/lib-autosave")
      config libFilePath =
        [ appWindowTitle "renju-app"
        , appTheme darkTheme
        , appFontDefMem "Regular" $(embedResource "fonts/Roboto-Regular.ttf")
        , appInitEvent <. LoadLib <| libPathOrDef libFilePath
        , appDisposeEvent <| SaveCurrentLib
        , appWindowState <| MainWindowNormal (630, 630)
        , appWindowResizable False
        ]

  args <- getArgs
  pickSubcommand args <| \libFilePath -> startApp (model <| libPathOrDef libFilePath) (handleEvent appCfg) (buildUI appCfg) (config libFilePath)
 where
  -- chances are, there's a cleaner way to write this
  _stoneTextures :: StoneTypes Texture
  _stoneTextures =
    StoneTypes
      { _blank = $(embedImage textureNames._blank)
      , _black = $(embedImage textureNames._black)
      , _white = $(embedImage textureNames._white)
      , _blackDot = $(embedImage textureNames._blackDot)
      , _whiteDot = $(embedImage textureNames._whiteDot)
      }

  _boardTexture :: Texture
  _boardTexture = $(embedImage "board")

model file =
  AppModel
    { _libStates = URList.one Lib.empty
    , _editing = NotEditing
    , _readOnly = False
    , _currentFile = file
    }
