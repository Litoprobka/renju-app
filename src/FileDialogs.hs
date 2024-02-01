-- a wrapper around tinyfiledialogs

module FileDialogs where

import DefaultImports
import UITypes

import Graphics.UI.TinyFileDialogs
import Monomer
import Prelude qualified as P (head)

{-
--openFileDialog "owo" "~" [] "" False
  inputBox "toast" "le hidden toast" (Just "toast?")
-}

openLib :: Text -> IO AppEvent
openLib dataHome =
  maybe NOOP (LoadLib . P.head) -- if a file is picked, the list is guaranteed to contain one element
    <$> openFileDialog
      "Open a lib file"
      dataHome
      ["*.json", "*", "!*.*"]
      "Lib fiels"
      False

saveLib :: Text -> IO AppEvent
saveLib dataHome =
  maybe NOOP SaveLib
    <$> saveFileDialog
      "Save the lib"
      dataHome
      ["*.json", "*", "!*.*"]
      "Lib files"

-- editBoardText
editBoardText m t =
  Task <|
    maybe StopEditing (SavEditingBoardText m)
      <$> inputBox "Enter board text" "" (Just t)
