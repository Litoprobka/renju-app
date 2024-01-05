{-# LANGUAGE ViewPatterns #-}

module UI (buildUI) where

import DefaultImports
import EventHandling (handleClipboard)
import Lib (Lib)
import Lib qualified
import Move (Move)
import Move qualified
import MoveSeq (Stone (..))
import MoveSeq qualified
import UITypes

import Monomer

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
    & traverse
      ( \y ->
          [0 .. 14]
            |> map (`Move.fromIntPartial` y)
            .> traverse (boardNode l)
      )
    <&> reverse
    .> map hgrid
    .> vgrid
-- monadic mess

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

defaultShortcuts :: AppModel -> [(Text, AppEvent)]
defaultShortcuts model =
  [ ("C-r", Rotate)
  , ("C-m", Mirror)
  , ("Delete", LibChanging RemovePos)
  , ("C-t", Comment <| MoveSeq.toGetpos <| model ^. lib . Lib.moves)
  , ("C-z", LibChanging Undo)
  , ("C-S-z", LibChanging Redo)
  , ("C-y", LibChanging Redo)
  , ("C-o", LoadLibDialog)
  , ("C-s", SaveLibDialog) -- placeholder
  , ("C-c", Getpos)
  , ("C-v", Paste)
  , ("C-p", Screenshot)
  , ("C-h", ResetHistory)
  , ("C-S-r", ToggleReadOnly)
  , ("C-S-m", ToggleReadOnly)
  ]