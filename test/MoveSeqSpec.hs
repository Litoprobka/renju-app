{-# LANGUAGE ViewPatterns #-}
module MoveSeqSpec (spec) where

import DefaultImports
import Test.Hspec
import MoveSeq

-- | partial version of MoveSeq.fromGetpos
fgp :: Text -> MoveSeq
fgp (MoveSeq.fromGetpos -> Just ms) = ms
fgp _ = error "parsing MoveSeq failed"

spec :: Spec -- I need to declare Arbitraty instances for most of my types to test stuff properly
spec = do
    describe "==" $ do
        it "should work for some simple examples" $ do
            MoveSeq.empty `shouldBe` MoveSeq.empty
            fgp "h8i9j6" `shouldBe` fgp "j6i9h8"
            fgp "h8h9j7f9" `shouldBe` fgp "j7f9h8h9"
        it "should work with mirroring and rotations" $ do
            fgp "h8i9j6" `shouldBe` fgp "h8i7f6"
            fgp "h8i9j6" `shouldBe` fgp "f6g9h8"
        it "mirror" $  do
            fgp "h8i9j7" `shouldNotBe` fgp "h8i9b7"
            fgp "h8i9j7" `shouldNotBe` fgp "h8i9c7"
            fgp "h8i9j7" `shouldNotBe` fgp "h8i9d7"
            fgp "h8i9j7" `shouldNotBe` fgp "h8i9e7"
            fgp "h8i9j7" `shouldNotBe` fgp "h8i9f7"
            fgp "h8i9j8" `shouldNotBe` fgp "h8i9e8"
        