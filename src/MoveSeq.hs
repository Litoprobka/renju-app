{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module MoveSeq where

import DefaultImports

import Data.Sequence qualified as Seq
import Data.Text (snoc, toLower)

import Control.Exception (PatternMatchFail (PatternMatchFail))
import Control.Lens (lens)
import Data.Bits (Bits, (.&.), (.|.))
import Move (Move, Transformations)
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
  , _hashes :: Transformations LongHash
  -- ^ LongHashes of all positions symmetrical to the current one. Minimum of these is *the* LongHash of a position.
  }
  deriving (Show)

-- | A numeric representation of move list w/o move order, used to compare and hash MoveSeq's
newtype LongHash = LongHash Natural deriving (Show, Eq, Ord, Num, Bits, Hashable)

data Stone
  = Black
  | White
  deriving (Show, Eq, Ord, Enum)

makeLenses ''MoveSeq
hash :: MoveSeq -> LongHash
hash = fromMaybe 0 <. minimumOf (hashes . folded)

-- * Instances
instance Eq MoveSeq where
  (==) = (==) `on` hash

-- * Utility functions for Stone

flipStone :: Stone -> Stone
flipStone Black = White
flipStone White = Black

hashMult :: Num a => Stone -> a
hashMult Black = 0b01
hashMult White = 0b10

-- * Construction

-- | /O(1)./ A MoveSeq with no moves.
empty :: MoveSeq
empty = MoveSeq Seq.Empty Seq.Empty (pure 0)

{- | /O(1)./ Add a move with given coordinates to the position; if the coordinates are taken, return Nothing

>>> pos "h8i9h6" |> makeMove (move "h9") |> fmap toGetpos
Just "h8i9h6h9"
>>> pos "h8i9h6" |> makeMove (move "i9") |> fmap toGetpos
Nothing
-}
makeMove :: Move -> MoveSeq -> Maybe MoveSeq
makeMove move pos
  | move `notIn` pos =
      pos
        |> updateHashes Add move
        |> over nextColorMoves (Seq.:|> move)
        |> Just
  | otherwise = Nothing

{- | /O(1)./ Like 'makeMove', but returns the same position if the coordinates are taken

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

{- | /O(n)./ Construct a MoveSeq from an oldest-last list of moves

>>> fromList [Move.Move 7 7, Move.Move 8 8, Move.Move 9 5]
MoveSeq {_blackMoves = fromList [Move {_x = 7, _y = 7},Move {_x = 9, _y = 5}], _whiteMoves = fromList [Move {_x = 8, _y = 8}], _hashes = Vector [LongHash 231584178501592337514292610186187249950270747065061589676824193528848600530944,LongHash 14474011181624471095097012922214505206009281675045899760189053501748683997184,LongHash 497323236409786642182342195014838735427496950327112609656746446630975443716486090194944,LongHash 1942668892225729097879408586528362172082783453743349041304289664484187655236887773184,LongHash 1942669123809907572511799421116128718685237632611739526170084911194644782135078027264,LongHash 497323236424260653337006719241917853021209154366026096197306191820632267494626126987264,LongHash 26959946868017895328502301167095795134486154570532229723608647204864,LongHash 26959946679704843639584795771402356580195794430113736401950410276864]}
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
nextColor :: MoveSeq -> Stone
nextColor = moveCount .> (odd .> bool Black White) -- white if odd

-- | /O(1)./ Color of the last made move
prevColor :: MoveSeq -> Stone
prevColor = nextColor .> flipStone

{- | /O(n)./ A left-to-right list of moves of the position
>>> moveList <$> fromGetpos "h8i9j8k8"
Just [Move {_x = 7, _y = 7},Move {_x = 8, _y = 8},Move {_x = 9, _y = 7},Move {_x = 10, _y = 7}]
-}
moveList :: MoveSeq -> [Move]
moveList pos = alternate (pos ^.. blackMoves . folded) (pos ^.. whiteMoves . folded)
 where
  alternate (x : xs) ys = x : alternate ys xs
  alternate [] [] = []
  alternate [] _ = bug <| PatternMatchFail "blackMoves and whiteMoves differ in length by >1"

{- | /O(n)./ Returns the index of a move in a position, counting from one; Nothing if the does not exist in the position

This should have been an AffineFold, but they are not a thing in Lens
-}
moveIndex :: Move -> Fold MoveSeq Int
moveIndex move =
  combinePls -- seems like <> can only be used with folds when the underlying functor is known
    (indexIn blackMoves . to (+ 1))
    (indexIn whiteMoves . to (+ 2))
 where
  indexIn = (. folding (Seq.elemIndexL move) . to (* 2))
  combinePls f1 f2 k s = f1 k s *> f2 k s

{- | /~O(1)./ Returns the stone color of a move in a position, or Nothing if it does not contain the move

Unlike MoveSeq.moveIndex, this function uses the LongHash representation

>>> preview (stoneAt (Move.Move 8 8)) <$> fromGetpos "h8i9j6"
Just (Just White)
>>> preview (stoneAt (Move.Move 8 10)) <$> fromGetpos "h8i9j6"
Just Nothing
-}
stoneAt :: Move -> Fold MoveSeq Stone
stoneAt move =
  hashes
    . to Move.origin
    . folding bitLookup
 where
  bitLookup idHash
    | match == hashPart * hashMult Black = Just Black
    | match == hashPart * hashMult White = Just White
    | otherwise = Nothing
   where
    hashPart = LongHash <| Move.hashPart move
    mask = hashPart * (hashMult Black .|. hashMult White) -- matches both black and white stones
    match = idHash .&. mask

{-
>>> h8 = LongHash <| Move.hashPart <| move "h8"
>>> h8
>>> lh = pos "h8i9" |> view hashes |> (`S.index` 0)
>>> lh .&. (0b11 * h8)
LongHash 26959946667150639794667015087019630673637144422540572481103610249216
LongHash 26959946667150639794667015087019630673637144422540572481103610249216
-}

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

{- | /~O(n)./ Pretty-print a MoveSeq
Like with mapEmpty, complexity is actually linear (225).
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
  pickLens (nextColor -> Black) = onB
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
  pos |> over hashes (liftA2 zf Move.transformations)
 where
  zf trns = flip op (LongHash <. (* hashMult color) <. Move.hashPart <| trns move)
  (op, color) = case hu of
    Add -> ((+), pos |> nextColor)
    Remove -> ((-), pos |> prevColor)

{- | /~O(n)./ Apply a function to each Move that is not present in a given position.

Technically, complexity is linear (225), but in practice it is worse than /O(n)/, because n <= 225
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
