{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

module EventHandling (handleClipboard, handleEvent) where

import CLI
import DefaultImports
import MoveSeq qualified
import UITypes

import Data.Default
import Data.Sequence qualified as Seq
import Monomer
import Monomer.Lens qualified as L
import Monomer.Widgets.Container (
  Container (..),
  createContainer,
 )

import Data.Text (takeWhileEnd)
import FileDialogs
import Lib (Lib)
import Lib qualified
import Relude.Extra (traverseToSnd)
import System.FilePath (pathSeparator)
import System.Process (callCommand)
import UndoRedoList qualified as URList

-- Path to `handleClipboard` widget. Used in the ugly workaround way to send GetClipboard request from `keystroke` and then route it to `handleClipboard`
-- I'm still not sure how it works, but it works
cbHandlerPath :: Path
cbHandlerPath = rootPath Seq.|> 0 Seq.|> 0

-- even though `handleClibboard` is a widget, it is only used to handle GetClipboard response, so it belongs in EventHandling
handleClipboard :: WidgetNode s AppEvent -> WidgetNode s AppEvent
handleClipboard innerWidget =
  defaultWidgetNode "clipboard" cbWidget
    |> L.children
      .~ one innerWidget
 where
  cbWidget = createContainer () def{containerHandleEvent}

  containerHandleEvent _ this _ event = case event of
    Clipboard (cbToText .> MoveSeq.fromGetpos -> Just ms) ->
      Just <. resultReqs this <| [RaiseEvent <. LibChanging <| Putpos ms]
    _ -> Nothing

  cbToText (ClipboardText text) = text
  cbToText ClipboardEmpty = ""

handleEvent
  :: Config
  -- ^ Data directory
  -> AppWenv
  -> AppNode
  -> AppModel
  -> AppEvent
  -> [AppEventResponse AppModel AppEvent]
handleEvent (Config{_dataHome}) wenv _ model evt = case evt of
  NOOP -> []
  LoadLib libFilePath ->
    [ Task <| uncurry NewLib <$> traverseToSnd (errorToException <=< loadLib) libFilePath
    , updateTitle libFilePath
    ]
  LoadLibDialog -> one <| Task openLib
  SaveLib libFilePath ->
    [ updateTitle libFilePath
    , Task <|
        NOOP
          <$ when (not <| Lib.isEmpty (model ^. lib)) (CLI.saveLib libFilePath (model ^. lib))
    , updateModel currentFile (const libFilePath)
    ]
  SaveLibDialog -> one <| Task FileDialogs.saveLib
  SaveCurrentLib -> one <. Event <| SaveLib (model ^. currentFile)
  NewLib newTitle newLib ->
    [ updateLib (const newLib)
    , updateTitle newTitle
    ]
  BoardClick m btn count -> [handleClick m btn count]
  StopEditing -> [updateModel editing (const <| NotEditing)]
  SavEditingBoardText m t ->
    [ Event StopEditing
    , Event <. LibChanging <| BoardText m t
    ]
  Rotate -> updateIfNotEditing Lib.rotate
  Mirror -> updateIfNotEditing Lib.mirror
  Comment t -> [updateLib <| Lib.addComment t]
  Getpos -> one <. Request <. SetClipboard <. ClipboardText <. MoveSeq.toGetpos <| model ^. lib . Lib.moves
  Paste ->
    one
      <. Request
      <. GetClipboard
      <. _wniWidgetId
      <. fromMaybe (error "cbHandlerPath is invalid")
      <| nodeInfoFromPath wenv cbHandlerPath
  Screenshot ->
    one <. Task <|
      NOOP
        <$ callCommand
          ( toString <|
              "import -window \"$(xdotool getwindowfocus -f)\" "
                <> _dataHome
                <> "/screenshot.png && xclip -sel clip -t image/png "
                <> _dataHome
                <> "/screenshot.png" -- TODO: use ImageMagick as a library, don't use the xdotools workaround
          )
  ToggleReadOnly -> [updateModel UITypes.readOnly not]
  ResetHistory -> [updateLibStates <| URList.one <. view URList.current]
  LibChanging event
    | not <| model ^. UITypes.readOnly -> handleLibChanging event
    | otherwise -> []
 where
  handleLibChanging = \case
    RemovePos -> [updateLib Lib.remove]
    BoardText m t -> [updateLib <| Lib.addBoardText m t]
    StartEditing m ->
      [ editBoardText m (model ^. lib |> Lib.getBoardText m .> fromMaybe " ")
      , updateModel editing (const <| EditingBoardText m)
      ]
    Putpos ms -> [updateLib <| Lib.addPosRec ms]
    Undo -> [updateLibStates URList.undo]
    Redo -> [updateLibStates URList.redo]
  --
  -- fourmoulu won't let me add a line break here
  --
  updateModel lens f = Model (model |> lens %~ f)
  updateLibStates = updateModel libStates
  updateLib = updateLibStates <. URList.update
  updateIfNotEditing f = whenAlt (not <| model ^. isEditing) [updateLib f]

  errorToException :: Either LibLoadError Lib -> IO Lib
  errorToException (Left err) = error <| show err
  errorToException (Right newLib) = pure newLib

  handleClick m BtnLeft count =
    updateLib <|
      ( case count of
          1 ->
            if model ^. UITypes.readOnly
              then Lib.nextMove
              else Lib.addMove
          _ -> Lib.toMove
      )
        m
  handleClick m BtnMiddle _ = Event <. LibChanging <| StartEditing m
  handleClick _ BtnRight _ = updateLib <| Lib.back

  updateTitle libFilePath =
    Request
      <. UpdateWindow
      <. WindowSetTitle
      <| "renju-app - "
        <> dropDir libFilePath

  dropDir = takeWhileEnd (/= pathSeparator)

  curPos' = model ^. lib . Lib.moves
  curPos = (MoveSeq.toGetpos curPos', curPos' ^. MoveSeq.nextColor)

  -- Like Applicative `when`, but returns a value
  whenAlt :: Alternative f => Bool -> f a -> f a
  whenAlt True action = action
  whenAlt False _ = empty