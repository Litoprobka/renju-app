module LitoUtils where

import Universum
import Flow
import qualified Data.Sequence as Seq
import Move(Move)
import qualified Move

-- various utility functions that don't belong anywhere. Module name is LitoUtils so it doesn't conflict with anything

infixl 9 <.>>
-- | lifted function composition
(<.>>) :: Functor f => (a -> f b) -> (b -> c) -> (a -> f c)
(<.>>) f1 f2 x = f1 x <&> f2

mapxy :: (Int -> Int -> a -> b) -> Seq (Seq a) -> Seq (Seq b)
mapxy f =
    Seq.mapWithIndex (\y -> Seq.mapWithIndex (`f` y))

map2d :: (a -> b) -> Seq (Seq a) -> Seq (Seq b)
map2d f = mapxy (\_ _ -> f)

-- | Given a function f and a value x, try applying f to x. If the result is Just y, return y; otherwise, return x
tryApply :: (a -> Maybe a) -> a -> a
tryApply f x = fromMaybe x (f x)

-- | Shared part of Pos.fromGetpos and MoveSeq.fromGetpos
fromGetpos' :: Text -> Maybe [Move]
fromGetpos' = (<> "a") .> foldl' f ("", []) .> snd .> sequence where

    f :: (String, [Maybe Move]) -> Char -> (String, [Maybe Move])
    f (acc, moves) c
        | c >= 'a' && c <= 'o' = ([c], if acc /= "" then Move.fromText (Universum.toText acc) : moves else moves) -- if we encounter a char, try to parse the current accumulator to Move, then append the result to moves. Reset the accumulator.
        | otherwise = (acc ++ [c], moves)                                                                         -- if we encounter a digit, add it to the accumulator.

applyAll :: [a -> a] -> a -> a
applyAll = foldl' (.>) id

-- Given a predicate p, function f and value x: if p x is true, return f x; otherwise, return x
applyIf :: (a -> Bool) -> (a -> a) -> a -> a
applyIf p f x = if p x then f x else x

applyIf2 :: (a -> b -> Bool) -> (a -> b -> b) -> a -> b -> b -- am I going insane?
applyIf2 p f x = applyIf (p x) (f x)
