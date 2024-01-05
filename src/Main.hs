{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

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

import Codec.Picture qualified as Picture
import Control.Concurrent.Async
import Data.Vector.Storable.ByteString (vectorToByteString)
import HSInstall.Paths (getShareDir)
import Monomer
import MoveSeq (Stone (..))
import Paths_renju_app (getDataDir)
import System.Directory (
  XdgDirectory (XdgData),
  createDirectoryIfMissing,
  getXdgDirectory,
 )

main :: IO ()
main = do
  resourceDir <- (<> "/resources/") <. fromString <$> getShareDir getDataDir

  _dataHome <-
    lookupEnv "RENJU_APP_DIR"
      & onNothingM (getXdgDirectory XdgData "renju-app")
      <&> fromString

  (_stoneTextures, _boardTexture) <- concurrently (loadStoneTextures resourceDir) (loadBoardTexture resourceDir)

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

loadStoneTextures :: Text -> IO (StoneTextureType -> (ByteString, Size))
loadStoneTextures resourceDir = do
  let names =
        [ "blank" -- Blank
        , "black-stone-gradient" -- NextMove Black
        , "white-stone-gradient" -- NextMove White
        , "move-exists-black" -- Dot Black
        , "move-exists-white" -- Dot White
        ]
  [blank, black, white, blackDot, whiteDot] <- mapConcurrently (loadImage resourceDir) names
  pure <| \case
    Blank -> blank
    NextMove Black -> black
    NextMove White -> white
    Dot Black -> blackDot
    Dot White -> whiteDot

loadBoardTexture :: Text -> IO (ByteString, Size)
loadBoardTexture resourceDir = loadImage resourceDir "board"

loadImage :: Text -> Text -> IO (ByteString, Size)
loadImage resourceDir name = do
  let path = resourceDir <> name <> ".png"
  Right dynImage <- Picture.readImage <| toString path
  let
    image@Picture.Image{imageWidth, imageHeight} = Picture.convertRGBA8 dynImage
    size = Size (fromIntegral imageWidth) (fromIntegral imageHeight)
    bytes = vectorToByteString <| Picture.imageData image
  pure (bytes, size)

-- if BS.null content
--  then throwIO $ ImageLoadFailed $ "Failed to load: " ++ path
--  else pure . Right $ content

-- imageState = ImageState source (Just (imgData, imgSize))

model file =
  AppModel
    { _libStates = URList.one Lib.empty
    , _editing = NotEditing
    , _readOnly = False
    , _currentFile = file
    }
