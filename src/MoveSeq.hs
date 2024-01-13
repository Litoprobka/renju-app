{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module MoveSeq where

import DefaultImports

import Data.Aeson
import Data.Aeson.Types (toJSONKeyText)
import Data.Sequence qualified as Seq
import Data.Text (snoc, toLower)
import Data.Vector.Generic.Sized qualified as S

import Control.Exception (PatternMatchFail (PatternMatchFail))
import Control.Lens (lens)
import Move (Move, Vec8)
import Move qualified

{- $setup
>>> pos = fromGetpos .> fromMaybe (error "invalid getpos")
>>> move = Move.fromText .> fromMaybe (error "invalid move")
-}

{- | A /seq/uence of /move/s, representing a position on the board.

Comparison ignores move order and rotation / mirroring.

This implementation works only for Renju/Gomoku (i.e. not Pente), because it assumes every odd move is black and every even move is white
-}
data MoveSeq = MoveSeq
  { _blackMoves :: Seq Move
  , _whiteMoves :: Seq Move
  , _hashes :: Vec8 LongHash
  -- ^ LongHashes of all positions symmetrical to the current one. Minimum of these is *the* LongHash of a position.
  }
  deriving (Show)

-- | A numeric representation of move list w/o move order, used to compare and hash MoveSeq's
newtype LongHash = LongHash Integer deriving (Show, Eq, Ord, Num, Hashable)

data Stone
  = Black
  | White
  deriving (Show, Eq, Ord, Enum)

makeLenses ''MoveSeq
hash :: Getting r MoveSeq LongHash
hash = to <| S.minimum <. view hashes

-- * Instances
instance Eq MoveSeq where
  (==) = (==) `on` view hash

instance Hashable MoveSeq where
  hashWithSalt salt = hashWithSalt salt <. view hash

instance ToJSON MoveSeq where
  toJSON = String <. toGetpos

instance ToJSONKey MoveSeq where
  toJSONKey = toJSONKeyText toGetpos
instance FromJSON MoveSeq where
  parseJSON = withText "MoveSeq" parser
   where
    parser (fromGetpos -> Just pos) = pure pos
    parser txt = fail <| "cannot parse " <> show txt <> " into MoveSeq"

instance FromJSONKey MoveSeq where
  fromJSONKey = FromJSONKeyTextParser parser
   where
    parser (fromGetpos -> Just pos) = pure pos
    parser k = fail <| "cannot parse key " <> show k <> " into MoveSeq"

-- * Utility functions for Stone

flipStone :: Stone -> Stone
flipStone Black = White
flipStone White = Black

hashMult :: Num a => Stone -> a
hashMult Black = 1
hashMult White = 2

-- * Construction

-- | /O(1)./ A MoveSeq with no moves.
empty :: MoveSeq
empty = MoveSeq Seq.Empty Seq.Empty (S.replicate z)
 where
  z = LongHash 0

{- | /O(n)./ Add a move with given coordinates to the position; if the coordinates are taken, return Nothing

>>> pos "h8i9h6" |> makeMove (move "h9") |> fmap toGetpos
Just "h8i9h6h9"
>>> pos "h8i9h6" |> makeMove (move "i9") |> fmap toGetpos
-}
makeMove :: Move -> MoveSeq -> Maybe MoveSeq
makeMove move pos
  | move `notIn` pos =
      pos
        |> updateHashes Add move
        |> over nextColorMoves (Seq.:|> move)
        |> Just
  | otherwise = Nothing

{- | /O(n)./ Like 'makeMove', but returns the same position if the coordinates are taken

>>> pos "h8i9h6" |> makeMove' (move "h9") |> toGetpos
"h8i9h6h9"
>>> pos "h8i9h6" |> makeMove' (move "i9") |> toGetpos
"h8i9h6"
-}
makeMove' :: Move -> MoveSeq -> MoveSeq
makeMove' move =
  tryApply (makeMove move)

{- | /O(1)./ Returns the positon without the last move, or the same position if it was empty

>>> toGetpos <| back MoveSeq.empty
""
>>> toGetpos <| back <| pos "h8h9j7"
"h8h9"
-}
back :: MoveSeq -> MoveSeq
back pos = case pos ^? prevColorMoves . _last of
  Nothing -> pos
  Just move ->
    pos
      |> updateHashes Remove move
      |> over prevColorMoves (dropR 1)
 where
  dropR n seq = Seq.take (Seq.length seq - n) seq

{- | /O(n)./ Given Move `m`, returns a position where `m` is the last move, or the same position if it does not contain `m`.

>>> pos "h8i9j8j9" |> toMove (move "i9") |> toGetpos
"h8i9"
>>> pos "h8i9j8j9" |> toMove (move "a1") |> toGetpos
"h8i9j8j9"
>>> pos "h8i9j6" |> toMove (move "h8") |> toGetpos
"h8"
-}
toMove :: Move -> MoveSeq -> MoveSeq
toMove move pos = fromMaybe pos <| go pos
 where
  go pos' =
    lastMove pos'
      >>= \move' ->
        if move' /= move
          then pos' |> back .> go
          else Just pos'

-- ** From other types

{- | /O(n^2)./ Construct a MoveSeq from a oldest-last list of moves
>>> fromList [Move.Move 7 7, Move.Move 8 8, Move.Move 9 5]
MoveSeq {_blackMoves = fromList [Move {_x = 7, _y = 7},Move {_x = 9, _y = 5}], _whiteMoves = fromList [Move {_x = 8, _y = 8}], _hashes = Vector [LongHash 23580369429369911338394552015355538614916560710659122865033044,LongHash 2620041291167985700123397777602357757623342334241116896163700,LongHash 507528786056415874612613683420470581219134060880924366960507065742100,LongHash 6265787482178244272013944906057979564862907875870528424044978216884,LongHash 6265811062547399749167562576921430505183289574768635907265415978164,LongHash 507528788676456891887739859864946325681685661891690138294537954980980,LongHash 273892859523678922033139806156612576562907086006924820,LongHash 273892757720723688564415218233217662603175902427271764]}
-}
fromList :: [Move] -> MoveSeq
fromList = flipfoldl' makeMove' MoveSeq.empty

{- | /O(n)./ Construct a MoveSeq from a string in getpos format ("h8i9j6i8k8")

>>> fromGetpos "h8i9j6" <&> MoveSeq.toText <&> error
15 - - - - - - - - - - - - - - -
14 - - - - - - - - - - - - - - -
13 - - - - - - - - - - - - - - -
12 - - - - - - - - - - - - - - -
11 - - - - - - - - - - - - - - -
10 - - - - - - - - - - - - - - -
 9 - - - - - - - - o - - - - - -
 8 - - - - - - - x - - - - - - -
 7 - - - - - - - - - - - - - - -
 6 - - - - - - - - - x - - - - -
 5 - - - - - - - - - - - - - - -
 4 - - - - - - - - - - - - - - -
 3 - - - - - - - - - - - - - - -
 2 - - - - - - - - - - - - - - -
 1 - - - - - - - - - - - - - - -
   a b c d e f g h i j k l m n o
-}
fromGetpos :: Text -> Maybe MoveSeq
fromGetpos =
  toLower
    .> (`snoc` 'a')
    .> toString
    .> foldl' f ("", [])
    .> snd
    .> reverse
    .> sequence
    <.>> fromList
 where
  f :: (Text, [Maybe Move]) -> Char -> (Text, [Maybe Move])
  f (acc, moves) c
    | isCoordLetter c = (one c, if acc /= "" then Move.fromText acc : moves else moves) -- if we encounter a char, try to parse the current accumulator to Move, then append the result to moves. Reset the accumulator.
    | otherwise = (acc `snoc` c, moves) -- if we encounter a digit, add it to the accumulator.
  isCoordLetter c = c >= 'a' && c <= 'o'

-- * Destruction

-- ** Queries

{- | /O(1)./ Checks if a MoveSeq is empty
>>> isEmpty MoveSeq.empty
True
>>> isEmpty <$> fromGetpos "h8h9h6"
Just False
-}
isEmpty :: MoveSeq -> Bool
isEmpty MoveSeq{_blackMoves = Seq.Empty, _whiteMoves = Seq.Empty} = True
isEmpty _ = False

{- | /O(1)./ The number of moves in a position
>>> moveCount MoveSeq.empty
0
>>> moveCount <$> fromGetpos "h8i9j6i8"
Just 4
-}
moveCount :: MoveSeq -> Int
moveCount pos = Seq.length (pos ^. blackMoves) + Seq.length (pos ^. whiteMoves)

{- | /O(1)./ Color of the to-be-made move
>>> view nextColor MoveSeq.empty
Black
>>> view nextColor <$> fromGetpos "h8i9j6"
Black
Just White
-}
nextColor :: Getter MoveSeq Stone
nextColor = to moveCount . to (odd .> bool Black White) -- white if odd

-- | /O(1)./ Color of the last made move
prevColor :: Getter MoveSeq Stone
prevColor = nextColor . to flipStone

{- | /O(n)./ A left-to-right list of moves of the position
>>> moveList <$> fromGetpos "h8i9j8k8"
Just [Move {_x = 7, _y = 7},Move {_x = 8, _y = 8},Move {_x = 9, _y = 7},Move {_x = 10, _y = 7}]
-}
moveList :: MoveSeq -> [Move]
moveList pos = alternate (pos ^.. blackMoves . each) (pos ^.. whiteMoves . each)
 where
  alternate (x : xs) ys = x : alternate ys xs
  alternate [] [] = []
  alternate [] _ = bug <| PatternMatchFail "blackMoves and whiteMoves differ in length by >1"

{- | /O(n)./ Returns the index of a move in a position, counting from one; Nothing if the does not exist in the position
this should have been an AffineFold, but they are not a thing in Lens
-}
moveIndex :: Move -> Fold MoveSeq Int
moveIndex move =
  combinePls -- seems like <> can only be used with folds when the underlying functor is known
    (indexIn blackMoves . to (+ 1))
    (indexIn whiteMoves . to (+ 2))
 where
  indexIn = (. folding (Seq.elemIndexL move) . to (* 2))
  combinePls f1 f2 k s = f1 k s *> f2 k s

{- | /O(n)./ Returns the stone color of a move in a position, or Nothing if it does not contain the move

>>> preview (stoneAt (Move.Move 8 8)) <$> fromGetpos "h8i9j6"
Just (Just White)
>>> preview (stoneAt (Move.Move 8 10)) <$> fromGetpos "h8i9j6"
Just Nothing
-}
stoneAt :: Move -> Fold MoveSeq Stone
stoneAt move =
  combinePls
    (findMove blackMoves . to (const Black))
    (findMove whiteMoves . to (const White))
 where
  findMove = (. folded . filtered (== move))
  combinePls f1 f2 k s = f1 k s *> f2 k s

-- | /O(n)./ Checks if a move does not exist in a position
notIn :: Move -> MoveSeq -> Bool
notIn move = hasn't <| stoneAt move

lastMove :: MoveSeq -> Maybe Move
lastMove = (^? prevColorMoves . _last)

-- ** Conversions

{- | /O(n)./ Convert a position to getpos format

>>> fromGetpos "h8i9j6" <&> toGetpos
Just "h8i9j6"
-}
toGetpos :: MoveSeq -> Text
toGetpos =
  moveList
    <.>> Move.toText
    .> fold

{- | /~O(n^2)./ Pretty-print a MoveSeq
Like with mapEmpty, complexity is actually linear (225n).
Currently used only for code in comments
-}
toText :: MoveSeq -> Text
toText pos =
  Move.grid
    <&> map (snoc " " <. (\m -> toXO <| pos ^? MoveSeq.stoneAt m))
    |> imap (\i -> foldl' (<>) <. align <| i + 1)
      .> (letters :)
      .> reverse
      .> unlines
 where
  align i
    | i > 9 = show i
    | otherwise = " " <> show i
  toXO Nothing = '-'
  toXO (Just Black) = 'x'
  toXO (Just White) = 'o'
  letters = "   a b c d e f g h i j k l m n o"

-- * Other

{- | use one lens on BlackNext and a different one on WhiteNext.
Used as a helper for prevColorMoves and nextColorMoves
-}
coloredLens
  :: Lens' MoveSeq a
  -- ^ when nextColor is Black
  -> Lens' MoveSeq a
  -- ^ when nextColor is White
  -> Lens' MoveSeq a
coloredLens onB onW = lens get set
 where
  pickLens (view nextColor -> Black) = onB
  pickLens _ = onW

  get pos = pos ^. pickLens pos
  set pos val = pos |> pickLens pos .~ val

{- | whiteMoves when nextColor is Black, blackMoves when it is White
>>> fromGetpos "h8i9j6" <&> view prevColorMoves
Just (fromList [Move {_x = 7, _y = 7},Move {_x = 9, _y = 5}])
-}
prevColorMoves :: Lens' MoveSeq (Seq Move)
prevColorMoves = coloredLens whiteMoves blackMoves

{- | blackMoves when nextColor is Black, whiteMoves when it is White

>>> fromGetpos "h8i9j6" <&> view nextColorMoves
Just (fromList [Move {_x = 8, _y = 8}])
-}
nextColorMoves :: Lens' MoveSeq (Seq Move)
nextColorMoves = coloredLens blackMoves whiteMoves

data HashUpdate
  = Add
  | Remove

{- | A helper function to update the hashes when changing a MoveSeq.
Should be called _before_ adding / removing the move itself, because
it assumes `nextColor` for `Add` and `prevColor` for `Remove`
-}
updateHashes :: HashUpdate -> Move -> MoveSeq -> MoveSeq
updateHashes hu move pos =
  pos |> over hashes (S.zipWith zf Move.transformations)
 where
  zf trns = flip op (fromIntegral <. (* hashMult color) <. Move.hashPart <| trns move)
  (op, color) = case hu of
    Add -> ((+), pos ^. nextColor)
    Remove -> ((-), pos ^. prevColor)

{- | /~O(n^2)./ Apply a function to each Move that is not present in a given position.

Technically, complexity is linear (225n), but in practice it is worse than /O(n^2)/, because n <= 225
...and that means using O notation here is kinda pointless, but whatever
-}
mapEmpty :: (Move -> a) -> MoveSeq -> [a]
mapEmpty f moves =
  Move.grid
    & concat
    & filter (`notIn` moves)
    <&> f

-- | /~O(n^2)./ Apply a function to all positions that can be derived from the current one
mapNext :: (MoveSeq -> a) -> MoveSeq -> [a]
mapNext f moves = mapEmpty (f <. (`MoveSeq.makeMove'` moves)) moves

{- | /O(n log n)./ Return all parent positions (current one minus one move)

>>> fromGetpos "h8i9j6i8k8" <&> allPrev .> map toGetpos
Just ["j6i9k8i8","h8i9k8i8","h8i9j6i8"]
>>> fromGetpos "h8h9h6i9i6i8" <&> allPrev .> map toGetpos
Just ["h8i9h6i8i6","h8h9h6i8i6","h8h9h6i9i6"]
-}
allPrev :: MoveSeq -> [MoveSeq]
allPrev pos =
  replicate (pos |> view prevColorMoves .> Seq.length) pos
    |> imap deleteAt
 where
  deleteAt i pos' =
    pos'
      |> updateHashes Remove (pos' ^?! prevColorMoves . ix i)
      |> over prevColorMoves (Seq.deleteAt i)


transform :: (Move -> Move) -> MoveSeq -> MoveSeq
transform f =
  moveList
    <.>> f
    .> fromList
{- ^ /O(n)./ Apply a given transformation (rotation or mirroring) to a MoveSeq

>>> fromGetpos "h8i9j6" <&> (\pos -> fmap (`transform` pos) Move.transformations) <&> foldMap MoveSeq.toText <&> error
15 - - - - - - - - - - - - - - -
14 - - - - - - - - - - - - - - -
13 - - - - - - - - - - - - - - -
12 - - - - - - - - - - - - - - -
11 - - - - - - - - - - - - - - -
10 - - - - - - - - - - - - - - -
 9 - - - - - - - - o - - - - - -
 8 - - - - - - - x - - - - - - -
 7 - - - - - - - - - - - - - - -
 6 - - - - - - - - - x - - - - -
 5 - - - - - - - - - - - - - - -
 4 - - - - - - - - - - - - - - -
 3 - - - - - - - - - - - - - - -
 2 - - - - - - - - - - - - - - -
 1 - - - - - - - - - - - - - - -
   a b c d e f g h i j k l m n o
15 - - - - - - - - - - - - - - -
14 - - - - - - - - - - - - - - -
13 - - - - - - - - - - - - - - -
12 - - - - - - - - - - - - - - -
11 - - - - - - - - - - - - - - -
10 - - - - - - - - - - - - - - -
 9 - - - - - - o - - - - - - - -
 8 - - - - - - - x - - - - - - -
 7 - - - - - - - - - - - - - - -
 6 - - - - - x - - - - - - - - -
 5 - - - - - - - - - - - - - - -
 4 - - - - - - - - - - - - - - -
 3 - - - - - - - - - - - - - - -
 2 - - - - - - - - - - - - - - -
 1 - - - - - - - - - - - - - - -
   a b c d e f g h i j k l m n o
15 - - - - - - - - - - - - - - -
14 - - - - - - - - - - - - - - -
13 - - - - - - - - - - - - - - -
12 - - - - - - - - - - - - - - -
11 - - - - - - - - - - - - - - -
10 - - - - - - - - - x - - - - -
 9 - - - - - - - - - - - - - - -
 8 - - - - - - - x - - - - - - -
 7 - - - - - - - - o - - - - - -
 6 - - - - - - - - - - - - - - -
 5 - - - - - - - - - - - - - - -
 4 - - - - - - - - - - - - - - -
 3 - - - - - - - - - - - - - - -
 2 - - - - - - - - - - - - - - -
 1 - - - - - - - - - - - - - - -
   a b c d e f g h i j k l m n o
15 - - - - - - - - - - - - - - -
14 - - - - - - - - - - - - - - -
13 - - - - - - - - - - - - - - -
12 - - - - - - - - - - - - - - -
11 - - - - - - - - - - - - - - -
10 - - - - - x - - - - - - - - -
 9 - - - - - - - - - - - - - - -
 8 - - - - - - - x - - - - - - -
 7 - - - - - - o - - - - - - - -
 6 - - - - - - - - - - - - - - -
 5 - - - - - - - - - - - - - - -
 4 - - - - - - - - - - - - - - -
 3 - - - - - - - - - - - - - - -
 2 - - - - - - - - - - - - - - -
 1 - - - - - - - - - - - - - - -
   a b c d e f g h i j k l m n o
15 - - - - - - - - - - - - - - -
14 - - - - - - - - - - - - - - -
13 - - - - - - - - - - - - - - -
12 - - - - - - - - - - - - - - -
11 - - - - - - - - - - - - - - -
10 - - - - - x - - - - - - - - -
 9 - - - - - - - - o - - - - - -
 8 - - - - - - - x - - - - - - -
 7 - - - - - - - - - - - - - - -
 6 - - - - - - - - - - - - - - -
 5 - - - - - - - - - - - - - - -
 4 - - - - - - - - - - - - - - -
 3 - - - - - - - - - - - - - - -
 2 - - - - - - - - - - - - - - -
 1 - - - - - - - - - - - - - - -
   a b c d e f g h i j k l m n o
15 - - - - - - - - - - - - - - -
14 - - - - - - - - - - - - - - -
13 - - - - - - - - - - - - - - -
12 - - - - - - - - - - - - - - -
11 - - - - - - - - - - - - - - -
10 - - - - - - - - - x - - - - -
 9 - - - - - - o - - - - - - - -
 8 - - - - - - - x - - - - - - -
 7 - - - - - - - - - - - - - - -
 6 - - - - - - - - - - - - - - -
 5 - - - - - - - - - - - - - - -
 4 - - - - - - - - - - - - - - -
 3 - - - - - - - - - - - - - - -
 2 - - - - - - - - - - - - - - -
 1 - - - - - - - - - - - - - - -
   a b c d e f g h i j k l m n o
15 - - - - - - - - - - - - - - -
14 - - - - - - - - - - - - - - -
13 - - - - - - - - - - - - - - -
12 - - - - - - - - - - - - - - -
11 - - - - - - - - - - - - - - -
10 - - - - - - - - - - - - - - -
 9 - - - - - - - - - - - - - - -
 8 - - - - - - - x - - - - - - -
 7 - - - - - - - - o - - - - - -
 6 - - - - - x - - - - - - - - -
 5 - - - - - - - - - - - - - - -
 4 - - - - - - - - - - - - - - -
 3 - - - - - - - - - - - - - - -
 2 - - - - - - - - - - - - - - -
 1 - - - - - - - - - - - - - - -
   a b c d e f g h i j k l m n o
15 - - - - - - - - - - - - - - -
14 - - - - - - - - - - - - - - -
13 - - - - - - - - - - - - - - -
12 - - - - - - - - - - - - - - -
11 - - - - - - - - - - - - - - -
10 - - - - - - - - - - - - - - -
 9 - - - - - - - - - - - - - - -
 8 - - - - - - - x - - - - - - -
 7 - - - - - - o - - - - - - - -
 6 - - - - - - - - - x - - - - -
 5 - - - - - - - - - - - - - - -
 4 - - - - - - - - - - - - - - -
 3 - - - - - - - - - - - - - - -
 2 - - - - - - - - - - - - - - -
 1 - - - - - - - - - - - - - - -
   a b c d e f g h i j k l m n o
-}
