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
  stoneImage = do
    textures <- view stoneTextures
    let (texture, size) = textures stoneAsset
    pure <| imageMem_ ("stone" <> show stoneAsset) texture size [alignCenter, alignMiddle, fitEither]

  stoneAsset = case l ^. Lib.moves |> MoveSeq.stoneAt m of
    Nothing
      | moveText /= ""
          || noNextMove ->
          Blank
      | otherwise -> Dot <| currentPos ^. MoveSeq.nextColor
    Just color -> NextMove color

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
boardImage = do
  (texture, size) <- view boardTexture
  pure <| imageMem_ "background" texture size [fitFill]

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
buildUI cfg _ model =
  usingReader cfg <| do
    bi <- boardImage
    bg <- boardGrid (model ^. lib)
    pure
      <. keystroke (defaultShortcuts model)
      <. handleClipboard
      <| zstack [bi, bg]

defaultShortcuts :: AppModel -> [(Text, AppEvent)]
defaultShortcuts model =
  [ ("C-r", Rotate)
  , ("C-m", Mirror)
  , ("Delete", LibChanging RemovePos)
  , ("C-t", Comment <. MoveSeq.toGetpos <| model ^. lib . Lib.moves)
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