{-# LANGUAGE TemplateHaskell #-}
module TH where

import DefaultImports
import IO (decodeTexture)
import Data.FileEmbed (embedFile)
import System.FilePath ((</>))
import Language.Haskell.TH

resourceDir :: FilePath
resourceDir = "hsinstall/share/renju-app/resources/"

embedResource path = embedFile $ resourceDir </> path

embedImage :: Text -> Q Exp
embedImage path = [| decodeTexture $ $(embedResource $ (<> ".png") $ toString path)|]