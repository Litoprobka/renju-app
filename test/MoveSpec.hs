{-# LANGUAGE ViewPatterns #-}
module MoveSpec (ft, spec) where

import DefaultImports
import Test.Hspec
import Test.Hspec.QuickCheck
import Move

-- | Partial version of Move.fromText
ft :: Text -> Move
ft (Move.fromText -> Just m) = m
ft _ = error "failed to parse Move"

spec :: Spec
spec = do
    describe "fromInt" $ do
        prop "returns Nothing only for out-of-bounds coordinates" $
            \x y -> fromInt x y `shouldBe` if x `elem` [0..14] && y `elem` [0..14] then (Just (fromIntPartial x y) :: Maybe Move) else (Nothing :: Maybe Move)
    -- seems like nothing else can be properly tested without defining custom Arbitrary instances

    describe "toByte / fromByte" $ do
        it "toByte .> fromByte === id" $ do
            (ft "j9" |> toByte |> fromBytePartial) `shouldBe` ft "j9"