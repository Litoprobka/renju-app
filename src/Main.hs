{-# LANGUAGE ViewPatterns #-}

module Main where

import DefaultImports
import qualified Lib
import Lib (Lib)
import qualified MoveSeq
import MoveSeq (Stone(..), MoveSeq)
import qualified Move
import Move (Move)
import CLI (loadLib, saveLib, LibLoadError(..))
import qualified UndoRedoList as URList
import System.Hclip
import UITypes
import BoardTextEditor (boardTextEditor)

import Monomer

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
    | (currentPos |> MoveSeq.lastMove) == Just m = green -- current move
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
    zstack [
      boardImage,
      boardGrid (model ^. lib),
      btEditor `nodeVisible` model ^. isEditing
      
    ]

  btEditor = boardTextEditor boardTextNodeOrDef (model ^. lib |> Lib.getBoardText boardTextNodeOrDef |> fromMaybe "")

  boardTextNodeOrDef = case model ^. editing of
    EBoardText m -> m
    _ -> Move.fromIntPartial 0 0


handleEvent
  :: AppWenv
  -> AppNode
  -> AppModel
  -> AppEvent
  -> [AppEventResponse AppModel AppEvent]
handleEvent _ _ model evt = case evt of
  Blank -> []
  LoadDefaultLib -> oneTask <| NewLib <$> (throwError =<< loadLib "lib-autosave")
  
  SaveDefaultLib -> oneTask <| Blank <$
    when (not <| Lib.isEmpty (model ^. lib)) (saveLib "lib-autosave" (model ^. lib))

  NewLib newLib -> updateLib (const newLib)
  
  BoardClick m btn count -> handleClick m btn count
  RemovePos -> updateLib Lib.remove

  BoardText m t -> updateLib <| Lib.addBoardText m t

  StartEditing m -> updateModel editing (const <| EBoardText m)
  StopEditing -> updateModel editing (const <| ENone)

  Rotate -> updateIfNotEditing Lib.rotate
  Mirror -> updateIfNotEditing Lib.mirror
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
    updateIfNotEditing f = join [ updateLib f | not <| model ^. isEditing ] -- the use of join is a bit of ugly

    throwError :: Either LibLoadError Lib -> IO Lib
    throwError (Left err) = error <| show err
    throwError (Right newLib) = pure newLib

    putposErr :: Maybe MoveSeq -> IO MoveSeq
    putposErr = maybe (error "Failed to parse MoveSeq") pure

    handleClick m BtnLeft count = updateLib <| case count of
      1 -> Lib.addMove m
      _ -> Lib.toMove m
    handleClick m BtnMiddle _ = one <| Event <| StartEditing m
    handleClick _ BtnRight _ = updateLib <| Lib.back

    oneTask = one <. Task

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
  , ("C-s", SaveDefaultLib) -- placeholder
  , ("C-c", Getpos)
  , ("C-v", Paste)
  ]
