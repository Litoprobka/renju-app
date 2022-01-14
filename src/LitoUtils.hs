module LitoUtils where

import DefaultImports
import Data.List.Index (imap)
import qualified Data.HashMap.Strict as HashMap

-- various utility functions that don't belong anywhere. Module name is LitoUtils so it doesn't conflict with anything

infixl 9 <.>>
-- | lifted function composition
(<.>>) :: Functor f => (a -> f b) -> (b -> c) -> (a -> f c)
(<.>>) f1 f2 x = f1 x <&> f2

infixr 9 <: -- should it be <<. or <.. ?
-- | composes a two-argument function with a one argument function
(<:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(<:) f1 f2 x = f1 <. f2 x

infixl 9 ..> -- :> is taken
-- | composes a two-argument function with a one argument function
(..>) :: (a -> b -> c) -> (c -> d) -> a -> b -> d
(..>) f1 f2 x = f1 x .> f2

mapxy :: (Int -> Int -> a -> b) -> [[a]] -> [[b]]
mapxy f =
    imap (\y -> imap (`f` y))

map2d :: (a -> b) -> [[a]] -> [[b]]
map2d f = mapxy (\_ _ -> f)

-- | Given a function f and a value x, try applying f to x. If the result is Just y, return y; otherwise, return x
tryApply :: (a -> Maybe a) -> a -> a
tryApply f x = fromMaybe x (f x)                                                                        -- if we encounter a digit, add it to the accumulator.

applyAll :: [a -> a] -> a -> a
applyAll = foldl' (.>) id

-- Given a predicate p, function f and value x: if p x is true, return f x; otherwise, return x
applyIf :: (a -> Bool) -> (a -> a) -> a -> a
applyIf p f x = if p x then f x else x

applyIf2 :: (a -> b -> Bool) -> (a -> b -> b) -> a -> b -> b -- am I going insane?
applyIf2 p f x = applyIf (p x) (f x)

partitionWithKey :: Hashable k => (k -> v -> Bool) -> HashMap k v -> (HashMap k v, HashMap k v)
partitionWithKey p = HashMap.foldrWithKey' f (HashMap.empty, HashMap.empty) where
    f k v maps@(_, _) =
       maps |> (if p k v then _1 else _2) %~ HashMap.insert k v

partition :: Hashable k => (v -> Bool) -> HashMap k v -> (HashMap k v, HashMap k v)
partition p = partitionWithKey <| const p
        
        
