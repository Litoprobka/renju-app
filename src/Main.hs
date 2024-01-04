{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import CLI (
  LibLoadError (..),
  loadLib,
  pickSubcommand,
  saveLib,
 )
import DefaultImports
import FileDialogs
import Lib (Lib)
import Lib qualified
import Move (Move)
import Move qualified
import MoveSeq (MoveSeq, Stone (..))
import MoveSeq qualified
import UITypes
import UndoRedoList qualified as URList

import Data.Default
import Data.Sequence qualified as Seq
import Data.Text (takeWhileEnd)
import HSInstall.Paths (getShareDir)
import Monomer
import Monomer.Lens qualified as L
import Monomer.Widgets.Container (Container (..), createContainer)
import Paths_renju_app (getDataDir)
import System.Directory (
  XdgDirectory (XdgData),
  createDirectoryIfMissing,
  getXdgDirectory,
 )
import System.FilePath (pathSeparator)
import System.Process (callCommand)

imageWithDir :: WidgetEvent e => Text -> [ImageCfg e] -> App (WidgetNode s e)
imageWithDir path cfg = do
  dir <- getResourcesDir
  pure <| image_ (dir <> path) cfg

boardNode :: Lib -> Move -> App AppNode
boardNode l m =
  tooltip'
    <. box_ [onBtnPressed <| BoardClick m]
    <. zstack
    <$> sequence
      [ stoneImage
      , pure <| label_ moveText [ellipsis, trimSpaces, multiline] `styleBasic` [textCenter, textMiddle, textColor color]
      ]
 where
  currentPos = view Lib.moves l
  stoneImage = imageWithDir ("/resources/" <> stoneAsset <> ".png") [alignCenter, alignMiddle, fitEither]
  stoneAsset = case l ^. Lib.moves |> MoveSeq.stoneAt m of
    Nothing
      | moveText /= ""
          || noNextMove ->
          "blank"
      | otherwise ->
          "move-exists-" <> case currentPos ^. MoveSeq.nextColor of
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
  [0 .. 14]
    <&> ( \y ->
            [0 .. 14]
              <&> (`Move.fromIntPartial` y)
              <&> boardNode l
        )
    & reverse
    <&> sequence
    & sequence
    <&> map hgrid
    <&> vgrid

-- Path to `handleClipboard` widget. Used in the ugly workaround way to send GetClipboard request from `keystroke` and then route it to `handleClipboard`
-- I'm still not sure how it works, but it works
cbHandlerPath :: Path
cbHandlerPath = rootPath Seq.|> 0 Seq.|> 0

handleClipboard :: WidgetNode s AppEvent -> WidgetNode s AppEvent
handleClipboard innerWidget =
  defaultWidgetNode "clipboard" cbWidget
    |> L.children
    .~ one innerWidget
 where
  cbWidget = createContainer () def{containerHandleEvent}

  containerHandleEvent _ this _ event = case event of
    Clipboard (cbToText .> MoveSeq.fromGetpos -> Just ms) ->
      Just
        <| WidgetResult
          { _wrNode = this
          , _wrRequests = one <| RaiseEvent <| Putpos ms
          }
    _ -> Nothing

  cbToText (ClipboardText text) = text
  cbToText ClipboardEmpty = ""

buildUI
  :: Config
  -> AppWenv
  -> AppModel
  -> AppNode
buildUI cfg _ model = widgetTree
 where
  widgetTree =
    keystroke (defaultShortcuts model)
      <| handleClipboard
      <| zstack
      <| usingReader cfg
      <| sequence
      <| [ boardImage
         , boardGrid (model ^. lib)
         ]

handleEvent
  :: Config
  -- ^ Data directory
  -> AppWenv
  -> AppNode
  -> AppModel
  -> AppEvent
  -> [AppEventResponse AppModel AppEvent]
handleEvent (Config _ dataHome) wenv _ model evt = case evt of
  NOOP -> []
  LoadLib libFilePath ->
    [ Task <| NewLib <$> (throwError =<< loadLib libFilePath)
    , updateTitle libFilePath
    ]
  LoadLibDialog -> oneTask openLib
  SaveLib libFilePath ->
    updateTitle libFilePath
      : ( Task <| NOOP
            <$ when (not <| Lib.isEmpty (model ^. lib)) (CLI.saveLib libFilePath (model ^. lib))
        )
      : updateModel currentFile (const libFilePath)
  SaveLibDialog -> oneTask FileDialogs.saveLib
  SaveCurrentLib -> one <| Event <| SaveLib (model ^. currentFile)
  NewLib newLib -> updateIfNotReadOnly (const newLib)
  BoardClick m btn count -> handleClick m btn count
  RemovePos -> updateIfNotReadOnly Lib.remove
  BoardText m t -> updateIfNotReadOnly <| Lib.addBoardText m t
  StartEditing m ->
    editBoardText m (model ^. lib |> Lib.getBoardText m |> fromMaybe " ")
      : (whenNotReadOnly <| updateModel editing (const <| EBoardText m))
  StopEditing -> updateModel editing (const <| ENone)
  SaveBoardText m t -> [Event StopEditing, Event <| BoardText m t]
  Rotate -> updateIfNotEditing Lib.rotate
  Mirror -> updateIfNotEditing Lib.mirror
  Comment t -> updateLib <| Lib.addComment t
  Getpos -> one <| Request <| SetClipboard <| ClipboardText <| MoveSeq.toGetpos <| model ^. lib . Lib.moves
  Paste ->
    one
      <| Request
      <| GetClipboard
      <| _wniWidgetId
      <| fromMaybe (error "cbHandlerPath is invalid")
      <| nodeInfoFromPath wenv cbHandlerPath
  Putpos ms -> updateIfNotReadOnly <| Lib.addPosRec ms
  Undo -> whenNotReadOnly <| updateLibStates URList.undo
  Redo -> whenNotReadOnly <| updateLibStates URList.redo
  Screenshot ->
    oneTask <| NOOP
      <$ callCommand
        ( toString
            <| "import -window \"$(xdotool getwindowfocus -f)\" "
            <> dataHome
            <> "/screenshot.png && xclip -sel clip -t image/png "
            <> dataHome
            <> "/screenshot.png" -- TODO: use ImageMagick as a library, don't use the xdotools workaround
        )
  ToggleReadOnly -> updateModel UITypes.readOnly not
  ResetHistory -> updateLibStates <| URList.one <. view URList.current
 where
  updateModel lens f = [Model (model |> lens %~ f)]
  updateLibStates = updateModel libStates
  updateLib = updateLibStates <. URList.update
  updateIfNotEditing f = whenAlt (not <| model ^. isEditing) <| updateLib f
  throwError :: Either LibLoadError Lib -> IO Lib
  throwError (Left err) = error <| show err
  throwError (Right newLib) = pure newLib

  handleClick m BtnLeft count =
    updateLib
      <| ( case count of
            1 ->
              if model ^. UITypes.readOnly
                then Lib.nextMove
                else Lib.addMove
            _ -> Lib.toMove
         )
        m
  handleClick m BtnMiddle _ = one <| Event <| StartEditing m
  handleClick _ BtnRight _ = updateLib <| Lib.back

  oneTask = one <. Task

  whenNotReadOnly = whenAlt (not <| model ^. UITypes.readOnly)
  updateIfNotReadOnly = whenNotReadOnly <. updateLib

  updateTitle libFilePath =
    Request
      <| UpdateWindow
      <| WindowSetTitle
      <| "renju-app - "
      <> dropDir libFilePath

  dropDir = takeWhileEnd (/= pathSeparator)

  -- Like Applicative `when`, but returns a value
  whenAlt :: Alternative f => Bool -> f a -> f a
  whenAlt True action = action
  whenAlt False _ = empty

main :: IO ()
main = do
  rDir <- fromString <$> getShareDir getDataDir

  dataHome <-
    lookupEnv "RENJU_APP_DIR"
      & onNothingM (getXdgDirectory XdgData "renju-app")
      <&> fromString

  createDirectoryIfMissing True (toString dataHome) -- create $RENJU_APP_DIR / $XDG_DATA_HOME/renju-app
  let appCfg = Config rDir dataHome
      libPathOrDef = fromMaybe (dataHome <> "/lib-autosave")
      config libFilePath =
        [ appWindowTitle "renju-app"
        , appTheme darkTheme
        , appFontDef "Regular" <| rDir <> "/resources/fonts/Roboto-Regular.ttf"
        , appInitEvent <| LoadLib <| libPathOrDef libFilePath
        , appDisposeEvent <| SaveCurrentLib
        , appWindowState <| MainWindowNormal (765, 765)
        , appWindowResizable False
        ]

  args <- getArgs
  pickSubcommand args <| \libFilePath -> startApp (model <| libPathOrDef libFilePath) (handleEvent appCfg) (buildUI appCfg) (config libFilePath)
 where
  model file =
    AppModel
      { _libStates = URList.one Lib.empty
      , _editing = ENone
      , _readOnly = False
      , _currentFile = file
      }

  onNothingM = flip whenNothingM

defaultShortcuts :: AppModel -> [(Text, AppEvent)]
defaultShortcuts model =
  [ ("C-r", Rotate)
  , ("C-m", Mirror)
  , ("Delete", RemovePos)
  , ("C-t", Comment <| MoveSeq.toGetpos <| model ^. lib . Lib.moves)
  , ("C-z", Undo)
  , ("C-S-z", Redo)
  , ("C-y", Redo)
  , ("C-o", LoadLibDialog)
  , ("C-s", SaveLibDialog) -- placeholder
  , ("C-c", Getpos)
  , ("C-v", Paste)
  , ("C-p", Screenshot)
  , ("C-h", ResetHistory)
  , ("C-S-r", ToggleReadOnly)
  , ("C-S-m", ToggleReadOnly)
  ]