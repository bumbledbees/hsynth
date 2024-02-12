module Sound.Backend.UtilSpec ( spec ) where

import Data.Int

import Test.Hspec

import Sound.Backend.Util


maxIntNBoundSpec :: Spec
maxIntNBoundSpec = describe "maxIntNBound" $ do
    it "is equivalent to maxBound :: IntN" $ do
        maxIntNBound 8  `shouldBe` (maxBound :: Int8)
        maxIntNBound 16 `shouldBe` (maxBound :: Int16)
        maxIntNBound 32 `shouldBe` (maxBound :: Int32)
        maxIntNBound 64 `shouldBe` (maxBound :: Int64)


minIntNBoundSpec :: Spec
minIntNBoundSpec = describe "minIntNBound" $ do
    it "is equivalent to maxBound :: IntN" $ do
        minIntNBound 8  `shouldBe` (minBound :: Int8)
        minIntNBound 16 `shouldBe` (minBound :: Int16)
        minIntNBound 32 `shouldBe` (minBound :: Int32)
        minIntNBound 64 `shouldBe` (minBound :: Int64)


spec :: Spec
spec = do
    maxIntNBoundSpec
    minIntNBoundSpec
