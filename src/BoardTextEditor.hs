{-# LANGUAGE RecordWildCards #-}
module BoardTextEditor where

import DefaultImports

import Move (Move)
import Monomer
import UITypes

boardTextEditor :: Move -> Text -> AppNode
boardTextEditor m bt = compositeV "boardTextEditor" (BTModel m bt) (const Blank) buildUI handleEvent

buildUI :: BTWenv -> BTModel -> BTNode
buildUI _ _ =
    box_ [alignMiddle, alignCenter] <|
    themeSwitch_ darkTheme [themeClearBg] <| -- TODO: add curved edges
    flip styleBasic [maxHeight 120, maxWidth 150, padding 10] <|
    
    keystroke [("Enter", Save), ("Esc", Cancel)] <|
    vstack [
      label "Enter board text" `styleBasic` [textCenter, textMiddle],
      spacer,
      textField boardText,
      spacer,
      hstack [
        mainButton "Save" Save,
        filler,
        button "Cancel" Cancel
      ]
    ]

handleEvent :: EventHandler BTModel BTEvent AppModel AppEvent
handleEvent _ _ BTModel{..} evt = case evt of
  Save -> [
    Report StopEditing,
    Report <| BoardText _move _boardText
    ]
  Cancel -> one <| Report StopEditing