module LitoUtils where

import Universum
import Flow
import qualified Data.Sequence as Seq
import Monomer.Common.Lens (HasX(x))

infixl 9 <.>>
-- | lifed function composition
(<.>>) :: Functor f => (a -> f b) -> (b -> c) -> (a -> f c)
(<.>>) f1 f2 x = f1 x <&> f2

mapxy :: (Int -> Int -> a -> b) -> Seq (Seq a) -> Seq (Seq b)
mapxy f =
    Seq.mapWithIndex (\y -> Seq.mapWithIndex (`f` y))

map2d :: (a -> b) -> Seq (Seq a) -> Seq (Seq b)
map2d f = mapxy (\_ _ -> f)

-- | for testing purposes only

