{-# LANGUAGE NamedFieldPuns #-}

module Main where

import CLI (
  pickSubcommand,
 )
import DefaultImports
import EventHandling (handleEvent)
import Lib qualified
import UI (buildUI)
import UITypes
import UndoRedoList qualified as URList

import HSInstall.Paths (getShareDir)
import Monomer
import Paths_renju_app (getDataDir)
import System.Directory (
  XdgDirectory (XdgData),
  createDirectoryIfMissing,
  getXdgDirectory,
 )

main :: IO ()
main = do
  rDir <- fromString <$> getShareDir getDataDir

  dataHome <-
    lookupEnv "RENJU_APP_DIR"
      & onNothingM (getXdgDirectory XdgData "renju-app")
      <&> fromString

  createDirectoryIfMissing True (toString dataHome) -- create $RENJU_APP_DIR / $XDG_DATA_HOME/renju-app
  let appCfg = Config rDir dataHome
      libPathOrDef = fromMaybe (dataHome <> "/lib-autosave")
      config libFilePath =
        [ appWindowTitle "renju-app"
        , appTheme darkTheme
        , appFontDef "Regular" <| rDir <> "/resources/fonts/Roboto-Regular.ttf"
        , appInitEvent <| LoadLib <| libPathOrDef libFilePath
        , appDisposeEvent <| SaveCurrentLib
        , appWindowState <| MainWindowNormal (765, 765)
        , appWindowResizable False
        ]

  args <- getArgs
  pickSubcommand args <| \libFilePath -> startApp (model <| libPathOrDef libFilePath) (handleEvent appCfg) (buildUI appCfg) (config libFilePath)
 where
  model file =
    AppModel
      { _libStates = URList.one Lib.empty
      , _editing = NotEditing
      , _readOnly = False
      , _currentFile = file
      }

  onNothingM = flip whenNothingM
