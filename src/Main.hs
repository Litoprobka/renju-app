{-# LANGUAGE TemplateHaskell #-}

module Main where

import DefaultImports
import qualified Lib
import Lib (Lib)
import qualified MoveSeq
import qualified Move
import Move (Move)
import Pos (Stone(..))
import Debug
import CLI

import Monomer

newtype AppModel = AppModel {
  _lib :: Lib.Lib
} deriving (Eq, Show)

data AppEvent
  = AppInit
  | BoardClick Move
  | MoveBack
  | RemovePos
  deriving (Eq, Show)

makeLenses 'AppModel

menuBarStyle :: CmbStyleBasic t => [t] -> [t]
menuBarStyle = map (`styleBasic` [
  radius 0
  -- bgColor darkGray,
  -- hlColor  lightGray,
  -- sndColor lightGray
  ])

-- a placeholder button
phButton :: Text -> WidgetNode s AppEvent
phButton name = button name AppInit

boardButton :: Lib -> Move -> WidgetNode s AppEvent
boardButton l m = boardButtonStyle (l ^. Lib.moves |> MoveSeq.stoneAt m) <| button (Move.toText m) <| BoardClick m

boardButtonStyle :: CmbStyleBasic t => Stone -> t -> t
boardButtonStyle stone = flip styleBasic [
  radius 0,
  case stone of
    None  -> bgColor gray
    Black -> bgColor darkGray
    White -> bgColor lightGray
  ] 

buttonGrid :: Lib -> WidgetNode s AppEvent
buttonGrid l =
  [0..14]
  <&> (\y ->
    [0..14]
    <&> (`Move.fromIntPartial` y)
    <&> boardButton l
  )
  |> reverse
  |> map hgrid
  |> vgrid

buildUI
  :: WidgetEnv AppModel AppEvent
  -> AppModel
  -> WidgetNode AppModel AppEvent
buildUI wenv model = widgetTree where
  widgetTree = vstack [
      hstack <| menuBarStyle [
        phButton "File",
        phButton "Edit",
        phButton "View",
        phButton "Move",
        button "Back" MoveBack
      ],
      separatorLine,
      spacer,
      buttonGrid (model ^. lib)
    ]

handleEvent
  :: WidgetEnv AppModel AppEvent
  -> WidgetNode AppModel AppEvent
  -> AppModel
  -> AppEvent
  -> [AppEventResponse AppModel AppEvent]
handleEvent wenv node model evt = case evt of
  AppInit -> []
  BoardClick m -> updateLibWith <| Lib.addMove m
  MoveBack -> updateLibWith Lib.back
  RemovePos -> updateLibWith Lib.remove
  where
    updateLibWith f = [ Model (model |> lib %~ f) ]

-- main = startRepl

main :: IO ()
main = do
  startApp model handleEvent buildUI config
  where
    config = [
      appWindowTitle "R",
      appTheme darkTheme,
      appFontDef "Regular" "./assets/fonts/Roboto-Regular.ttf",
      appInitEvent AppInit
      ]
    model = AppModel Lib.empty
