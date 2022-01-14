{-# LANGUAGE TemplateHaskell #-}

module Main where

import DefaultImports
import qualified Lib
import Lib (Lib)
import qualified MoveSet
import qualified MoveSeq
import MoveSet (Stone(..))
import qualified Move
import Move (Move)
import Data.Text(toLower)
import qualified Data.HashMap.Strict as HashMap

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
  | BoardText Move Text
  deriving (Eq, Show)

type AppWenv = WidgetEnv AppModel AppEvent
type AppNode = WidgetNode AppModel AppEvent

makeLenses 'AppModel

boardBox :: Lib -> HashMap Move Int -> Move -> AppNode
boardBox l mi m =
  box_ [expandContent, onBtnPressed handleClick] <|
  zstack [
    stoneImage,
    label_ moveText [ellipsis, trimSpaces, multiline] `styleBasic` [textCenter, textMiddle, textColor green]
  ]
  where
  currentPos = l^. Lib.moveSet
  stoneImage = image_ ("./assets/" <> stoneAsset <> ".png") [alignCenter, fitHeight]
  stoneAsset = case currentPos |> MoveSet.stoneAt m of
    Nothing -> if not <| Lib.exists (MoveSet.makeMove' m currentPos) l then
        "blank"
      else
        ("move-exists-" <>) <| toLower <| show <| currentPos ^. MoveSet.nextColor

    Just Black -> "black-stone"
    Just White -> "white-stone"

  handleClick BtnLeft _ = BoardClick m
  handleClick _ _ = MoveBack -- middle mouse button does not work for some reason

  moveText =
    Lib.getBoardText m l
    |> fromMaybe (maybe "" show (HashMap.lookup m mi))

boardImage :: AppNode
boardImage = image_ "./assets/board.png" [fitFill]

boardGrid :: Lib -> AppNode
boardGrid l =
  [0..14]
  <&> (\y ->
    [0..14]
    <&> (`Move.fromIntPartial` y)
    <&> boardBox l moveIndices
  )
  |> DefaultImports.reverse
  |> map hgrid
  |> vgrid
  where
    moveIndices :: HashMap Move Int
    moveIndices =
      l ^. Lib.moves . MoveSeq.moveList
      |> foldr' (\ m (i, l') -> (i + 1, HashMap.insert m i l')) (1, HashMap.empty)
      |> snd

buildUI
  :: AppWenv
  -> AppModel
  -> AppNode
buildUI _ model = widgetTree where
  widgetTree = keystroke [("Left", MoveBack), ("C-r", Rotate), ("Delete", RemovePos), ("Backspace", RemovePos)] <|
    zstack <| [
      boardImage,
      boardGrid (model ^. lib)
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
  BoardText m t -> updateLibWith <| Lib.addBoardText m t -- placeholder
  Rotate -> updateLibWith Lib.rotate -- TODO: make it force a GUI redraw
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
      appInitEvent AppInit,
      appWindowState <| MainWindowNormal (765, 765),
      appWindowResizable False
      ]
    model = AppModel Lib.empty
