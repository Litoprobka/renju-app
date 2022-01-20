{-# LANGUAGE TemplateHaskell #-}
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
import UndoRedoList (UndoRedoList)
import qualified UndoRedoList as URList
import Lens.Micro (SimpleGetter)
import System.Clipboard

import Monomer

newtype AppModel = AppModel {
  _libStates :: UndoRedoList Lib
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
  | Comment Text
  | Getpos
  | Paste
  | Putpos MoveSeq
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
  Blank -> []
  LoadDefaultLib -> one <| Task <| NewLib <$> (throwError =<< loadLib "lib-autosave")
  
  SaveDefaultLib -> one <| Task <| Blank <$
    when (not <| Lib.isEmpty (model ^. lib)) (saveLib "lib-autosave" (model ^. lib))

  NewLib newLib -> updateLibWith (const newLib)
  
  BoardClick m btn _ -> updateLibWith <| handleClick m btn
  RemovePos -> updateLibWith Lib.remove

  BoardText m t -> updateLibWith <| Lib.addBoardText m t
  Rotate -> updateLibWith Lib.rotate
  Mirror -> updateLibWith Lib.mirror
  Comment t -> updateLibWith <| Lib.addComment t
  Getpos -> one <| Task <| Blank <$ setClipboardString (toString <| MoveSeq.toGetpos <| model ^. lib . Lib.moves)

  Paste -> one <| Task <| Putpos <$> 
    (getClipboardString
    <&> (=<<) (fromString .> MoveSeq.fromGetpos) -- this is anything but readable
    >>= putposErr)

  Putpos ms -> updateLibWith <| Lib.addPosRec ms
  Undo -> updateLibStates URList.undo
  Redo -> updateLibStates URList.redo
  where
    updateLibStates f = [ Model (model |> libStates %~ f ) ]
    updateLibWith = updateLibStates <. URList.update

    throwError :: Either LibLoadError Lib -> IO Lib
    throwError (Left err) = error <| show err
    throwError (Right newLib) = pure newLib

    putposErr :: Maybe MoveSeq -> IO MoveSeq
    putposErr = maybe (error "Failed to parse MoveSeq") pure

    handleClick m BtnLeft = Lib.addMove m
    handleClick m BtnMiddle = Lib.addBoardText m "board text"
    handleClick _ BtnRight = Lib.back

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