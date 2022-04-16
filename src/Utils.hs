module Utils where

import Relude
import Flow

-- * Utils
--
-- $utils
-- various utility functions that don't belong anywhere

infixl 9 <.>>
-- | lifted function composition
(<.>>) :: Functor f => (a -> f b) -> (b -> c) -> (a -> f c)
(<.>>) f1 f2 x = f1 x <&> f2

infixr 9 <..
-- | composes a two-argument function with a one argument function
(<..) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(<..) f1 f2 x = f1 <. f2 x

infixl 9 ..> -- :> is taken
-- | composes a two-argument function with a one argument function
(..>) :: (a -> b -> c) -> (c -> d) -> a -> b -> d
(..>) f1 f2 x = f1 x .> f2

-- | Given a function `f` and a value `x`, try applying `f` to `x`. If the result is `Just y`, return `y`; otherwise, return `x`
tryApply :: (a -> Maybe a) -> a -> a
tryApply f x = fromMaybe x (f x)

applyAll :: [a -> a] -> a -> a
applyAll = foldl' (.>) id

-- | Given a predicate `p`, function `f` and value `x`: if `p x` is true, return `f x`; otherwise, return `x`
applyIf :: (a -> Bool) -> (a -> a) -> a -> a
applyIf p f x = if p x then f x else x

applyWhen :: Bool -> (a -> a) -> a -> a
applyWhen b = applyIf <| const b

-- | Given a predicate `p`, function `f` and values `x` and `y`: if `p x y` is true, return `f x y`; otherwise, return `y`
applyIf2 :: (a -> b -> Bool) -> (a -> b -> b) -> a -> b -> b -- am I going insane?
applyIf2 p f x = applyIf (p x) (f x)

safeHead :: [a] -> Maybe a
safeHead = nonEmpty <.>> head

