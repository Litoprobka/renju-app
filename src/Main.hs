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
import System.Process (callCommand)
import UITypes
import BoardTextEditor (boardTextEditor)
import HSInstall.Paths (getShareDir)
import Paths_renju_app (getDataDir)
import System.Directory (createDirectoryIfMissing, getXdgDirectory, XdgDirectory(XdgData))

import Monomer

imageWithDir :: WidgetEvent e => Text -> [ImageCfg e] -> App (WidgetNode s e)
imageWithDir path cfg = do
  dir <- getResourcesDir
  pure <| image_ (dir <> path) cfg

boardNode :: Lib -> Move -> App AppNode
boardNode l m =
  tooltip'
  <. box_ [onBtnPressed <| BoardClick m]
  <. zstack <$> sequence [
    stoneImage,
    pure <| label_ moveText [ellipsis, trimSpaces, multiline] `styleBasic` [textCenter, textMiddle, textColor color]
  ]
  where
  currentPos = view Lib.moves l
  stoneImage = imageWithDir ("/resources/" <> stoneAsset <> ".png") [alignCenter, alignMiddle, fitEither]
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

boardImage :: App AppNode
boardImage = imageWithDir "/resources/board.png" [fitFill]

boardGrid :: Lib -> App AppNode
boardGrid l =
  [0..14]
  <&> (\y ->
    [0..14]
    <&> (`Move.fromIntPartial` y)
    <&> boardNode l
  )
  & reverse
  <&> sequence & sequence
  <&> map hgrid
  <&> vgrid

buildUI
  :: Config
  -> AppWenv
  -> AppModel
  -> AppNode
buildUI cfg _ model = widgetTree where
  widgetTree = keystroke (defaultShortcuts model) <|
    zstack <|
    usingReader cfg <|
    sequence <| [
      boardImage,
      boardGrid (model ^. lib),
      pure <| btEditor `nodeVisible` model ^. isEditing
    ]

  btEditor = boardTextEditor boardTextNodeOrDef (model ^. lib |> Lib.getBoardText boardTextNodeOrDef |> fromMaybe "")

  boardTextNodeOrDef = case model ^. editing of
    EBoardText m -> m
    _ -> Move.fromIntPartial 0 0


handleEvent
  :: Config -- ^ Data directory
  -> AppWenv
  -> AppNode
  -> AppModel
  -> AppEvent
  -> [AppEventResponse AppModel AppEvent]
handleEvent (Config _ dataHome) _ _ model evt = case evt of
  NOOP -> []
  LoadDefaultLib -> oneTask <| NewLib <$> (throwError =<< loadLib (dataHome <> "/lib-autosave"))

  SaveDefaultLib -> oneTask <| NOOP <$
    when (not <| Lib.isEmpty (model ^. lib)) (saveLib (dataHome <> "/lib-autosave") (model ^. lib))

  NewLib newLib -> updateIfNotReadOnly (const newLib)

  BoardClick m btn count -> handleClick m btn count
  RemovePos -> updateIfNotReadOnly Lib.remove

  BoardText m t -> updateIfNotReadOnly <| Lib.addBoardText m t

  StartEditing m -> whenNotReadOnly <| updateModel editing (const <| EBoardText m)
  StopEditing -> updateModel editing (const <| ENone)

  Rotate -> updateIfNotEditing Lib.rotate
  Mirror -> updateIfNotEditing Lib.mirror
  Comment t -> updateLib <| Lib.addComment t
  Getpos -> oneTask <| NOOP <$ setClipboard (toString <| MoveSeq.toGetpos <| model ^. lib . Lib.moves)

  Paste -> oneTask <| Putpos <$>
    (getClipboard
    <&> fromString .> MoveSeq.fromGetpos
    >>= putposErr)

  Putpos ms -> updateIfNotReadOnly <| Lib.addPosRec ms
  Undo -> whenNotReadOnly <| updateLibStates URList.undo
  Redo -> whenNotReadOnly <| updateLibStates URList.redo

  Screenshot -> oneTask <| NOOP <$ callCommand (toString <|
    "import -window \"$(xdotool getwindowfocus -f)\" " <> dataHome <> "/screenshot.png && xclip -sel clip -t image/png " <> dataHome <> "/screenshot.png") -- TODO: use ImageMagick as a library, don't use the xdotools workaround

  ToggleReadOnly -> updateModel readOnly not

  where
    updateModel lens f = [ Model (model |> lens %~ f) ]
    updateLibStates = updateModel libStates
    updateLib = updateLibStates <. URList.update
    updateIfNotEditing f = join [ updateLib f | not <| model ^. isEditing ] -- the use of join is a bit ugly

    throwError :: Either LibLoadError Lib -> IO Lib
    throwError (Left err) = error <| show err
    throwError (Right newLib) = pure newLib

    putposErr :: Maybe MoveSeq -> IO MoveSeq
    putposErr = maybe (error "Failed to parse MoveSeq") pure

    handleClick m BtnLeft count = updateLib <| (case count of
      1 -> if model ^. readOnly
        then Lib.nextMove
        else Lib.addMove
      _ -> Lib.toMove) m
    handleClick m BtnMiddle _ = one <| Event <| StartEditing m
    handleClick _ BtnRight _ = updateLib <| Lib.back

    oneTask = one <. Task
    
    whenNotReadOnly actions =
      if model ^. readOnly
        then [ Event <| NOOP ]
        else actions
    updateIfNotReadOnly = whenNotReadOnly <. updateLib


main :: IO ()
main = do
  rDir <- fromString <$> getShareDir getDataDir

  dataHome <- 
    lookupEnv "RENJU_APP_DIR"
     & onNothingM (getXdgDirectory XdgData "renju-app")
    <&> fromString

  createDirectoryIfMissing True (toString dataHome) -- create $RENJU_APP_DIR / $XDG_DATA_HOME/renju-app

  let appCfg = Config rDir dataHome
      config = [
          appWindowTitle "R",
          appTheme darkTheme,
          appFontDef "Regular" <| rDir <> "/resources/fonts/Roboto-Regular.ttf",
          appInitEvent LoadDefaultLib,
          appDisposeEvent SaveDefaultLib,
          appWindowState <| MainWindowNormal (765, 765),
          appWindowResizable False
        ]

  startApp model (handleEvent appCfg) (buildUI appCfg) config
  where
    model = AppModel (URList.one Lib.empty) ENone False
    onNothingM = flip whenNothingM

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
  , ("C-p", Screenshot)
  , ("C-S-r", ToggleReadOnly)
  , ("C-S-m", ToggleReadOnly)
  ]
