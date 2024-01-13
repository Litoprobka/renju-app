{-# LANGUAGE ViewPatterns #-}

module UI (buildUI, textureNames) where

import DefaultImports

import EventHandling (handleClipboard)
import Lib (Lib)
import Lib qualified
import Move (Move)
import Move qualified
import MoveSeq (Stone (..))
import MoveSeq qualified
import UITypes

import Monomer hiding (black, white)
import Monomer qualified as Color (black, green, white)

boardNode :: Lib -> Move -> App AppNode
boardNode l m = do
  image <- stoneImage
  pure
    <. tooltip'
    <. box_ [onBtnPressed <| BoardClick m]
    <| zstack
      [ image
      , label_ moveText [ellipsis, trimSpaces, multiline]
          `styleBasic` [textCenter, textMiddle, textColor textColor']
      ]
 where
  currentPos = view Lib.moves l
  stoneImage = do
    textures <- view stoneTextures
    let Texture texture size = textures ^. stoneLens
    pure <| imageMem_ ("stone" <> textureNames ^. stoneLens) texture size [alignCenter, alignMiddle, fitEither]

  stoneLens :: Getter (StoneTypes a) a
  stoneLens = case l ^. Lib.moves |> MoveSeq.stoneAt m of
    Nothing
      | moveText
          /= ""
          || noNextMove ->
          blank
      | otherwise -> dotL (currentPos ^. MoveSeq.nextColor)
    Just color -> stoneL color

  moveText =
    Lib.getBoardText m l
      <|> show <$> (MoveSeq.moveIndex m <| l ^. Lib.moves)
      |> fromMaybe ""

  noNextMove = not <| Lib.exists (MoveSeq.makeMove' m currentPos) l

  textColor'
    | (currentPos |> MoveSeq.lastMove) == Just m = Color.green -- current move
    | otherwise = case MoveSeq.moveIndex m currentPos of
        Just (even -> True) -> Color.black
        Just _ -> Color.white
        Nothing -- board text
          | noNextMove -> green
          | otherwise -> case currentPos ^. MoveSeq.nextColor of
              Black -> Color.black
              White -> Color.white

  tooltip' = case flip Lib.getCommentOf l <$> MoveSeq.makeMove m currentPos of -- MoveSeq.makeMove is useful for once
    Nothing -> id
    Just "" -> id
    Just comment -> tooltip comment

textureNames :: StoneTypes Text
textureNames =
  StoneTypes
    { _blank = "blank"
    , _black = "black-stone-gradient"
    , _white = "white-stone-gradient"
    , _blackDot = "move-exists-black"
    , _whiteDot = "move-exists-white"
    }

boardImage :: App AppNode
boardImage = do
  Texture texture size <- view boardTexture
  pure <| imageMem_ "background" texture size [fitFill]

boardGrid :: Lib -> App AppNode
boardGrid l =
  Move.grid
    & (traverse <. traverse) (boardNode l)
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