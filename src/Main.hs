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
import CLI (loadLib, saveLib, LibLoadError(..))
import UndoRedoList (UndoRedoList)
import qualified UndoRedoList as URList
import Lens.Micro (SimpleGetter)

import Monomer

newtype AppModel = AppModel {
  _libStates :: UndoRedoList Lib
} deriving (Eq, Show)

data AppEvent
  = LoadDefaultLib
  | SaveDefaultLib
  | Blank ()
  | NewLib Lib
  | BoardClick Move
  | Rotate
  | MoveBack
  | RemovePos
  | BoardText Move Text
  | Comment Text
  | Undo
  | Redo
  deriving (Eq, Show)

type AppWenv = WidgetEnv AppModel AppEvent
type AppNode = WidgetNode AppModel AppEvent

makeLenses 'AppModel

-- | Gets current lib state
lib :: SimpleGetter AppModel Lib
lib = libStates . URList.current

boardNode :: Lib -> Move -> AppNode
boardNode l m =
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
    Nothing -> if not <| Lib.exists (MoveSeq.makeMove' m currentPos) l then
        "blank"
      else
        "move-exists-" <> if currentPos ^. MoveSeq.nextColor == Black then "black" else "white"

    Just Black -> "black-stone-gradient"
    Just White -> "white-stone-gradient"

  handleClick BtnLeft _ = BoardClick m
  handleClick BtnMiddle _ = BoardText m "board text"
  handleClick BtnRight _ = MoveBack

  moveText =
    Lib.getBoardText m l
    |> fromMaybe (maybe "" show (MoveSeq.moveIndex m <| l ^. Lib.moves))

  color
    | ( currentPos ^. MoveSeq.moveList |> safeHead) == Just m = green -- current move
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
    <&> boardNode l
  )
  |> reverse
  |> map hgrid
  |> vgrid

buildUI
  :: AppWenv
  -> AppModel
  -> AppNode
buildUI _ model = widgetTree where
  widgetTree = keystroke (defaultShortcuts model) <|
    zstack [
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
  Blank () -> []
  LoadDefaultLib -> one <| Task <| NewLib <$> (throwError =<< loadLib "lib-autosave")
  SaveDefaultLib -> one <| Task <| Blank <$> saveLib "lib-autosave" (model ^. lib)
  NewLib newLib -> updateLibWith (const newLib)
  BoardClick m -> updateLibWith <| Lib.addMove m
  MoveBack -> updateLibWith Lib.back
  RemovePos -> updateLibWith Lib.remove
  BoardText m t -> updateLibWith <| Lib.addBoardText m t -- placeholder
  Rotate -> updateLibWith Lib.rotate -- TODO: make it force a GUI redraw
  Comment t -> updateLibWith <| Lib.addComment t
  Undo -> updateLibStates URList.undo
  Redo -> updateLibStates URList.redo
  where
    updateLibStates f = [ Model (model |> libStates %~ f ) ]
    updateLibWith = updateLibStates <. URList.update

    throwError :: Either LibLoadError Lib -> IO Lib
    throwError (Left err) = error <| show err
    throwError (Right newLib) = return newLib

main :: IO ()
main = do
  startApp model handleEvent buildUI config
  where
    config = [
      appWindowTitle "R",
      appTheme darkTheme,
      appFontDef "Regular" "./assets/fonts/Roboto-Regular.ttf",
      appInitEvent LoadDefaultLib,
      appDisposeEvent SaveDefaultLib,
      appWindowState <| MainWindowNormal (765, 765),
      appWindowResizable False
      ]
    model = AppModel <| URList.one Lib.empty

defaultShortcuts :: AppModel -> [(Text, AppEvent)]
defaultShortcuts model = [
    ("Left", MoveBack)
  , ("C-r", Rotate)
  , ("Delete", RemovePos)
  , ("C-c", Comment <| MoveSeq.toGetpos <| model ^. lib . Lib.moves)
  , ("C-z", Undo)
  , ("C-S-z", Redo)
  , ("C-y", Redo)
  ]