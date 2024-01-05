-- a wrapper around tinyfiledialogs

module FileDialogs where

import DefaultImports
import UITypes

import Graphics.UI.TinyFileDialogs
import Monomer
import qualified Prelude as P (head)

{-
--openFileDialog "owo" "~" [] "" False
  inputBox "toast" "le hidden toast" (Just "toast?")
-}

openLib :: IO AppEvent
openLib =
  maybe NOOP (LoadLib . P.head) -- if a file is picked, the list is guaranteed to contain one element
    <$> openFileDialog
      "Open a lib file"
      "$RENJU_APP_DIR"
      ["*.json", "*", "!*.*"]
      "Lib fiels"
      False

saveLib :: IO AppEvent
saveLib =
  maybe NOOP SaveLib
    <$> saveFileDialog
      "Save the lib"
      "$RENJU_APP_DIR"
      ["*.json", "*", "!*.*"]
      "Lib files"

-- editBoardText
editBoardText m t =
  Task
    <| maybe StopEditing (SavEditingBoardText m)
    <$> inputBox "Enter board text" "" (Just t)
