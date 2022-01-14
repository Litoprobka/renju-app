{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

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
  | BoardText Move Text
  | Comment Text
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
boardBox l m =
  tooltip' <|
  box_ [onBtnPressed handleClick] <|
  zstack [
    stoneImage,
    label_ moveText [ellipsis, trimSpaces, multiline] `styleBasic` [textCenter, textMiddle, textColor color]
  ]
  where
  currentPos = view Lib.moves l
  stoneImage = image_ ("./assets/" <> stoneAsset <> ".png") [alignCenter, fitHeight]
  stoneAsset = case l ^. Lib.moves |> MoveSeq.stoneAt m of
    None -> if not <| Lib.exists (MoveSeq.makeMove' m currentPos) l then
        "blank"
      else
        "move-exists-" <> if even <| MoveSeq.moveCount currentPos then "black" else "white"

    Black -> "black-stone-gradient"
    White -> "white-stone-gradient"

  handleClick BtnLeft _ = BoardClick m
  handleClick _ _ = MoveBack -- middle mouse button does not work for some reason

  moveText =
    Lib.getBoardText m l
    |> fromMaybe (maybe "" show (MoveSeq.moveIndex m <| l ^. Lib.moves))

  color
    | (MoveSeq.getMoves currentPos |> safeHead) == Just m = green -- current move
    | otherwise = case MoveSeq.moveIndex m currentPos of
      Just (even -> True) -> black
      Just _ -> white
      Nothing -> green -- board text

  tooltip' = case flip Lib.getCommentOf l <$> MoveSeq.makeMove m currentPos of -- MoveSeq.makeMove is useful for once
    Nothing -> id
    Just "" -> id
    Just comment -> tooltip comment

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
  widgetTree = keystroke [("Left", MoveBack), ("C-r", Rotate), ("Delete", RemovePos), ("C-c", Comment <| MoveSeq.toGetpos <| model ^. lib . Lib.moves)] <|
    vstack [
      {-hstack <| menuBarStyle [
        phButton "File",
        phButton "Edit",
        phButton "View",
        phButton "Move"
      ],
      separatorLine,
      spacer,-}
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
  BoardText m t -> updateLibWith <| Lib.addBoardText m t -- placeholder
  Rotate -> updateLibWith Lib.rotate -- TODO: make it force a GUI redraw
  Comment t -> updateLibWith <| Lib.addComment t
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
