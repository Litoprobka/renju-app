{-# LANGUAGE TemplateHaskell #-}

module Main where

import DefaultImports
import qualified Lib
import Lib (Lib)
import qualified MoveSeq
import MoveSeq (Stone(..))
import qualified Move
import Move (Move)

import Monomer

newtype AppModel = AppModel {
  _lib :: Lib.Lib
} deriving (Eq, Show)

data AppEvent
  = AppInit
  | BoardClick Move
  | Rotate
  | MoveBack
  | RemovePos
  deriving (Eq, Show)

type AppWenv = WidgetEnv AppModel AppEvent
type AppNode = WidgetNode AppModel AppEvent

makeLenses 'AppModel

menuBarStyle :: CmbStyleBasic t => [t] -> [t]
menuBarStyle = map (`styleBasic` [
  radius 0
  -- bgColor darkGray,
  -- hlColor  lightGray,
  -- sndColor lightGray
  ])

-- a placeholder button
phButton :: Text -> AppNode
phButton name = button name AppInit

boardBox :: Lib -> Move -> AppNode
boardBox l m = box_ [expandContent, onBtnPressed handleClick] stoneImage where

  stoneImage = image_ ("./assets/" <> stoneAsset <> ".png") [alignCenter, fitHeight]
  stoneAsset = case l ^. Lib.moves |> MoveSeq.stoneAt m of
    None -> "blank"
    Black -> "black-stone"
    White -> "white-stone"

  handleClick BtnLeft _ = BoardClick m
  handleClick _ _ = MoveBack -- middle mouse button does not work for some reason

boardImage :: AppNode
boardImage = image_ "./assets/board.png" [fitFill]

boardGrid :: Lib -> AppNode
boardGrid l =
  [0..14]
  <&> (\y ->
    [0..14]
    <&> (`Move.fromIntPartial` y)
    <&> boardBox l
  )
  |> reverse
  |> map hgrid
  |> vgrid

buildUI
  :: AppWenv
  -> AppModel
  -> AppNode
buildUI _ model = widgetTree where
  widgetTree = keystroke [("C-z", MoveBack), ("C-r", Rotate)] <|
    vstack [
      hstack <| menuBarStyle [
        phButton "File",
        phButton "Edit",
        phButton "View",
        phButton "Move"
      ],
      separatorLine,
      spacer,
        zstack <| [
          boardImage,
          boardGrid (model ^. lib)
        ]
    ]

handleEvent
  :: AppWenv
  -> AppNode
  -> AppModel
  -> AppEvent
  -> [AppEventResponse AppModel AppEvent]
handleEvent _ _ model evt = case evt of
  AppInit -> []
  BoardClick m -> updateLibWith <| Lib.addMove m
  MoveBack -> updateLibWith Lib.back
  RemovePos -> updateLibWith Lib.remove
  Rotate -> updateLibWith <| Lib.rotate -- TODO: make it force a GUI redraw
  where
    updateLibWith f = [ Model (model |> lib %~ f) ]

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
