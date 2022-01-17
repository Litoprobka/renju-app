{-# LANGUAGE ViewPatterns, BlockArguments #-}
module MoveSeqSpec (spec) where

import DefaultImports
import Test.Hspec
import MoveSeq
import Move (transformations)
import MoveSpec (ft) -- this is probably not a good practice

-- | partial version of MoveSeq.fromGetpos
fgp :: Text -> MoveSeq
fgp (MoveSeq.fromGetpos -> Just ms) = ms
fgp _ = error "parsing MoveSeq failed"

i5 :: MoveSeq
i5 = fgp "h8i9j6"

i4 :: MoveSeq
i4 = fgp "h8i9j7"

d11 :: MoveSeq
d11 = fgp "h8h9h6i10i6"

spec :: Spec -- I need to declare Arbitraty instances for most of my types to test stuff properly
spec = do
    describe "==" do
        it "should work for some simple examples" do
            MoveSeq.empty `shouldBe` MoveSeq.empty
            i5 `shouldBe` fgp "j6i9h8"
            fgp "h8h9j7f9" `shouldBe` fgp "j7f9h8h9"
        it "should work with mirroring and rotations" do
            i5 `shouldBe` fgp "h8i7f6"
            i5 `shouldBe` fgp "f6g9h8"
        it "mirror" do
            i4 `shouldNotBe` fgp "h8i9b7"
            i4 `shouldNotBe` fgp "h8i9c7"
            i4 `shouldNotBe` fgp "h8i9d7"
            i4 `shouldNotBe` fgp "h8i9e7"
            i4 `shouldNotBe` fgp "h8i9f7"
            fgp "h8i9j8" `shouldNotBe` fgp "h8i9e8"
    describe "transform" do
        it "should not break the hash" do
            forM_ transformations \f -> d11 `shouldBe` transform f d11
            forM_ transformations \f -> makeMove' (ft "i9") d11 `shouldBe` makeMove' (f <| ft "i9") (transform f d11)
        