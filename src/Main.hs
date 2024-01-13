{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import DefaultImports

import CLI (pickSubcommand)
import EventHandling (handleEvent)
import Lib qualified
import UI (buildUI, textureNames)
import UITypes
import UndoRedoList qualified as URList

import Monomer

import Codec.Picture qualified as Picture
import Control.Concurrent.Async
import Data.Vector.Storable.ByteString (vectorToByteString)
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
        , appWindowState <| MainWindowNormal (765, 765)
        , appWindowResizable False
        ]

  args <- getArgs
  pickSubcommand args <| \libFilePath -> startApp (model <| libPathOrDef libFilePath) (handleEvent appCfg) (buildUI appCfg) (config libFilePath)

loadImage :: Text -> Text -> IO Texture
loadImage resourceDir name = do
  let path = resourceDir <> name <> ".png"
  Right dynImage <- Picture.readImage <| toString path
  let image@Picture.Image{imageWidth, imageHeight} = Picture.convertRGBA8 dynImage
      size = Size (fromIntegral imageWidth) (fromIntegral imageHeight)
      bytes = vectorToByteString <| Picture.imageData image
  pure <| Texture bytes size

model file =
  AppModel
    { _libStates = URList.one Lib.empty
    , _editing = NotEditing
    , _readOnly = False
    , _currentFile = file
    }
