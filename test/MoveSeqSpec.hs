module MoveSeqSpec (spec) where

import Universum
import Test.Hspec
import MoveSeq

spec :: Spec -- need to declare Arbitraty instances for most of my types to test stuff properly
spec = do
    describe "placeholder" $ do
        it "placeholder" $ do
            MoveSeq.empty `shouldBe` MoveSeq.empty