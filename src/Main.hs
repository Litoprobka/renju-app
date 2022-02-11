{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import DefaultImports
import qualified Lib
import Lib (Lib)
import qualified MoveSeq
import MoveSeq (Stone(..), MoveSeq)
import qualified Move
import Move (Move)
import CLI (loadLib, saveLib, LibLoadError(..))
import UndoRedoList (UndoRedoList)
import qualified UndoRedoList as URList
import System.Hclip

import Monomer

data AppModel = AppModel {
  _libStates :: UndoRedoList Lib,
  _editing :: EditType
} deriving (Eq, Show)

data AppEvent
  = LoadDefaultLib
  | SaveDefaultLib
  | Blank
  | NewLib Lib
  | BoardClick Move Button Int
  | Rotate
  | Mirror
  | RemovePos
  | BoardText Move Text
  | StartEditing Move
  | StopEditing
  | Comment Text
  | Getpos
  | Paste
  | Putpos MoveSeq
  | Undo
  | Redo
  deriving (Eq, Show)

data EditType
  = ENone
  | EBoardText Move
  deriving (Eq, Show)

type AppWenv = WidgetEnv AppModel AppEvent
type AppNode = WidgetNode AppModel AppEvent

makeLenses 'AppModel

data BTEvent
  = Save
  | Cancel
  deriving (Show, Eq)

data BTModel = BTModel {
  _move :: Move,
  _boardText :: Text
} deriving (Show, Eq)

makeLenses 'BTModel

-- | Gets current lib state
lib :: Getting r AppModel Lib
lib = libStates . URList.current

boardNode :: Lib -> Move -> AppNode
boardNode l m =
  tooltip' <|
  box_ [onBtnPressed <| BoardClick m] <|
  zstack [
    stoneImage,
    label_ moveText [ellipsis, trimSpaces, multiline] `styleBasic` [textCenter, textMiddle, textColor color]
  ]
  where
  currentPos = view Lib.moves l
  stoneImage = image_ ("./assets/" <> stoneAsset <> ".png") [alignCenter, alignMiddle, fitEither]
  stoneAsset = case l ^. Lib.moves |> MoveSeq.stoneAt m of
    Nothing
      | moveText /= "" 
      || noNextMove -> "blank"
      | otherwise -> "move-exists-" <> case currentPos ^. MoveSeq.nextColor of
      Black -> "black"
      White -> "white"


    Just Black -> "black-stone-gradient"
    Just White -> "white-stone-gradient"

  moveText =
    Lib.getBoardText m l
    |> fromMaybe (maybe "" show (MoveSeq.moveIndex m <| l ^. Lib.moves))

  noNextMove = not <| Lib.exists (MoveSeq.makeMove' m currentPos) l

  color
    | (currentPos ^. MoveSeq.moveList |> safeHead) == Just m = green -- current move
    | otherwise = case MoveSeq.moveIndex m currentPos of
      Just (even -> True) -> black
      Just _ -> white
      Nothing -- board text
        | noNextMove -> green
        | otherwise -> case currentPos ^. MoveSeq.nextColor of
          Black -> black
          White -> white

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
    zstack <| boardTextEditor' <| [
      boardImage,
      boardGrid (model ^. lib)
    ]

  boardTextEditor' l = case model ^. editing of
    EBoardText m -> l ++ [ box_ [alignMiddle, alignCenter] <| boardTextEditor m (model ^. lib |> Lib.getBoardText m |> fromMaybe "") ]
    _ -> l

handleEvent
  :: AppWenv
  -> AppNode
  -> AppModel
  -> AppEvent
  -> [AppEventResponse AppModel AppEvent]
handleEvent _ _ model evt = case evt of
  Blank -> []
  LoadDefaultLib -> one <| Task <| NewLib <$> (throwError =<< loadLib "lib-autosave")
  
  SaveDefaultLib -> one <| Task <| Blank <$
    when (not <| Lib.isEmpty (model ^. lib)) (saveLib "lib-autosave" (model ^. lib))

  NewLib newLib -> updateLib (const newLib)
  
  BoardClick m btn _ -> handleClick m btn
  RemovePos -> updateLib Lib.remove

  BoardText m t -> updateLib <| Lib.addBoardText m t

  StartEditing m -> updateModel editing (const <| EBoardText m)
  StopEditing -> updateModel editing (const <| ENone)

  Rotate -> updateLib Lib.rotate
  Mirror -> updateLib Lib.mirror
  Comment t -> updateLib <| Lib.addComment t
  Getpos -> one <| Task <| Blank <$ setClipboard (toString <| MoveSeq.toGetpos <| model ^. lib . Lib.moves)

  Paste -> one <| Task <| Putpos <$> 
    (getClipboard
    <&> fromString .> MoveSeq.fromGetpos
    >>= putposErr)

  Putpos ms -> updateLib <| Lib.addPosRec ms
  Undo -> updateLibStates URList.undo
  Redo -> updateLibStates URList.redo
  where
    updateModel lens f = [ Model (model |> lens %~ f) ]
    updateLibStates = updateModel libStates
    updateLib = updateLibStates <. URList.update

    throwError :: Either LibLoadError Lib -> IO Lib
    throwError (Left err) = error <| show err
    throwError (Right newLib) = pure newLib

    putposErr :: Maybe MoveSeq -> IO MoveSeq
    putposErr = maybe (error "Failed to parse MoveSeq") pure

    handleClick m BtnLeft = updateLib <| Lib.addMove m
    handleClick m BtnMiddle = one <| Event <| StartEditing m --one <| Event <| BoardText m "board text"
    handleClick _ BtnRight = updateLib <| Lib.back

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
    model = AppModel (URList.one Lib.empty) ENone

defaultShortcuts :: AppModel -> [(Text, AppEvent)]
defaultShortcuts model = [
    ("C-r", Rotate)
  , ("C-m", Mirror)
  , ("Delete", RemovePos)
  , ("C-t", Comment <| MoveSeq.toGetpos <| model ^. lib . Lib.moves)
  , ("C-z", Undo)
  , ("C-S-z", Redo)
  , ("C-y", Redo)
  , ("C-c", Getpos)
  , ("C-v", Paste)
  ]

boardTextEditor :: Move -> Text -> AppNode
boardTextEditor m bt = compositeV "boardTextEditor" initModel (const Blank) buildUI' handleEvent' where
  initModel = BTModel m bt

  buildUI' _ _ = themeSwitch_ darkTheme [themeClearBg] <| widgets `styleBasic` [maxHeight 120, maxWidth 150, padding 10]
  widgets =
      keystroke [("Enter", Save)] <| 
      vstack [
        label "Enter board text" `styleBasic` [textCenter, textMiddle],
        spacer,
        textField boardText,
        spacer,
        hstack [
          button "Cancel" Cancel,
          filler,
          button "Save" Save
        ]
      ]

  handleEvent' :: EventHandler BTModel BTEvent AppModel AppEvent
  handleEvent' _ _ BTModel{..} evt = case evt of
    Save -> [
      Report StopEditing,
      Report <| BoardText _move _boardText
      ]
    Cancel -> one <| Report StopEditing
