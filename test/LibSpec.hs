module LibSpec (spec) where

import Universum
import Test.Hspec
import Lib
import qualified Move
import Flow

spec :: Spec
spec = do
    describe "remove" $ do
        it "works with empty position" $ do
            remove Lib.empty `shouldBe` Lib.empty
        it "adding a move and then removing yields the same lib" $ do
            (l |> addMove (Move.fromBytePartial 42) |> remove) `shouldBe` l
    where
        l = Lib.empty