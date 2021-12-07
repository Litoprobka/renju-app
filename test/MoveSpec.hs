module MoveSpec (spec) where

import Universum
import Test.Hspec
import Test.Hspec.QuickCheck
import Move

spec :: Spec
spec = do
    describe "fromInt" $ do
        prop "returns Nothing only for out-of-bounds coordinates" $
            \x y -> fromInt x y `shouldBe` if x `elem` [0..14] && y `elem` [0..14] then (Just (fromIntPartial x y) :: Maybe Move) else (Nothing :: Maybe Move)
            