module LibSpec (spec) where

import DefaultImports
import Test.Hspec
import Lib
import qualified Move

spec :: Spec
spec = do
    describe "remove" $ do
        it "works with empty position" $ do
            remove Lib.empty `shouldBe` Lib.empty
        it "adding a move and then removing yields the same lib" $ do
            (l |> addMove (Move.Move 7 4) |> remove) `shouldBe` l
    where
        l = Lib.empty