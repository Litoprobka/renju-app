{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import DefaultImports

import IO (pickSubcommand, loadImage)
import EventHandling (handleEvent)
import Lib qualified
import UI (buildUI, textureNames)
import UITypes
import UndoRedoList qualified as URList

import Monomer

import Control.Concurrent.Async
import System.Directory (XdgDirectory (XdgData), createDirectoryIfMissing, getXdgDirectory)

import HSInstall.Paths (getShareDir)
import Paths_renju_app (getDataDir)

main :: IO ()
main = do
  resourceDir <- (<> "/resources/") <. fromString <$> getShareDir getDataDir

  _dataHome <-
    lookupEnv "RENJU_APP_DIR"
      & onNothingM (getXdgDirectory XdgData "renju-app")
      <&> fromString

  (_stoneTextures, _boardTexture) <-
    concurrently
      (mapConcurrently (loadImage resourceDir) textureNames)
      (loadImage resourceDir "board")

  createDirectoryIfMissing True (toString _dataHome) -- create $RENJU_APP_DIR / $XDG_DATA_HOME/renju-app
  let appCfg = Config{..}
      libPathOrDef = fromMaybe (_dataHome <> "/lib-autosave")
      config libFilePath =
        [ appWindowTitle "renju-app"
        , appTheme darkTheme
        , appFontDef "Regular" <| resourceDir <> "fonts/Roboto-Regular.ttf"
        , appInitEvent <. LoadLib <| libPathOrDef libFilePath
        , appDisposeEvent <| SaveCurrentLib
        , appWindowState <| MainWindowNormal (630, 630)
        , appWindowResizable False
        ]

  args <- getArgs
  pickSubcommand args <| \libFilePath -> startApp (model <| libPathOrDef libFilePath) (handleEvent appCfg) (buildUI appCfg) (config libFilePath)

model file =
  AppModel
    { _libStates = URList.one Lib.empty
    , _editing = NotEditing
    , _readOnly = False
    , _currentFile = file
    }
