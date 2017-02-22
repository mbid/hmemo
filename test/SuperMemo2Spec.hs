module SuperMemo2Spec where

import SuperMemo2
import Test.Hspec
import Test.QuickCheck
import Data.List

main = hspec spec

spec :: Spec
spec = do
  describe "nextEFactor" $ do
    return undefined
    it "should not change if quality == 4" $ do
      forAll
        ((arbitrary :: Gen Rational) `suchThat` (>= 1.3))
        (\ef -> nextEFactor 4 ef === ef)
    it "should increase if quality > 4" $ do
      property $ \ef -> nextEFactor 5 ef >= (ef :: Rational)
    it "should always be >= 1.3" $ do
      property $ \q ef -> nextEFactor q (ef :: Rational) >= 1.3
  describe "eFactors" $ do
    it "should have the right length" $ do
      property $ \qs -> length (eFactors qs) == length qs + 1
    it "should be constantly 2.5 if all qualities are 4" $ do
      eFactors (replicate 4 4) `shouldBe` replicate 5 (2.5 :: Rational)
    it "should be increasing (!) if all qualities are 3" $ do
      let es = eFactors (replicate 4 3)
      sort es `shouldBe` es
  describe "intervals" $ do
    it "should have the right length" $ do
      property $ \qs -> length (intervals qs) == length qs + 1
    it "should work for some examples" $ do
      intervals [4, 3, 4] `shouldBe` [15, 6, 1, 0]
      intervals [5, 5, 2, 4, 3, 4] `shouldBe` [14, 6, 1, 15, 6, 1, 0]
