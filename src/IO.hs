{-# LANGUAGE LambdaCase #-}

module IO where

import DefaultImports

import Lib
import UITypes (Texture (Texture))

import Codec.Picture (Image (..))
import Codec.Picture qualified as Picture
import Control.Exception (throwIO)
import Data.Aeson (eitherDecode)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Vector.Storable.ByteString (vectorToByteString)
import Monomer (Size (Size))
import System.AtomicWrite.Writer.LazyByteString (atomicWriteFile)
import System.Directory (doesFileExist)

data LibLoadError
  = FileNotFound String
  | ParseFailure String
  deriving (Show, Eq)

instance Exception LibLoadError

loadLib :: Text -> IO (Either LibLoadError Lib)
loadLib filePath = do
  fileExists <- doesFileExist <| toString filePath
  if not fileExists
    then pure <. Left <. FileNotFound <| toString filePath
    else
      readFileLBS (toString filePath)
        <&> eitherDecode
        <&> first ParseFailure

loadLib' :: Text -> IO Lib
loadLib' =
  loadLib
    >=> \case
      Left err -> throwIO err
      Right lib -> pure lib

saveLib :: Text -> Lib -> IO ()
saveLib filePath =
  encodePretty
    .> atomicWriteFile (toString filePath)

loadImage :: Text -> Text -> IO Texture
loadImage resourceDir name = do
  let path = resourceDir <> name <> ".png"
  Right dynImage <- Picture.readImage <| toString path
  let image@Picture.Image{imageWidth, imageHeight} = Picture.convertRGBA8 dynImage
      size = Size (fromIntegral imageWidth) (fromIntegral imageHeight)
      bytes = vectorToByteString <| Picture.imageData image
  pure <| Texture bytes size

-- todo: use optparse-applicative?
pickSubcommand :: [String] -> (Maybe Text -> IO ()) -> IO ()
pickSubcommand args gui = case args of
  arg : rest
    | arg == "gui" -> gui <. Just <| toFileName rest
    | arg == "repair" -> repair <| toFileName rest
    | arg == "merge" -> merge rest
  _ -> gui Nothing
 where
  toFileName = map fromString .> unwords
  repair fileName = do
    lib <- loadLib' fileName
    saveLib fileName <| Lib.repair lib
    putTextLn <| "lib " <> fileName <> " repaired succesfully. Check it out via `renju-app gui " <> fileName
  merge =
    reverse
      .> uncons
      .> ( \case
            Nothing -> pass
            Just (output, inputs) ->
              reverse inputs
                <&> fromString
                <&> loadLib'
                & sequence
                <&> foldr Lib.merge Lib.empty
                  >>= saveLib (fromString output)
         )
