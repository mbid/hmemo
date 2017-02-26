module SerializeSpec where

import Test.Hspec
import Test.QuickCheck
import Serialize
import Types
import Data.Monoid
import qualified Data.Text as T
import Data.Maybe
import Util

main = hspec spec

spec :: Spec
spec = do
  describe "parseEntry" $ do
    it "should parse entries without date and qualities" $ do
      property $
        \(CsvValue back) (CsvValue front) -> 
        parseEntry (front <> "\t" <> back) ===
        Just (Card {back = back, front = front}, CardStats Nothing [])
    it "should parse entries with data and qualities" $ do
      let e = parseEntry "front\tback \t2301-11-03T11:33:22\t 120453"
      e `shouldSatisfy` isJust
      let Just (card, stats) = e
      back card `shouldBe` "back"
      front card `shouldBe` "front"
      lastReview stats `shouldSatisfy` isJust
      responseQualities stats `shouldBe` [3, 5, 4, 0, 2, 1]
    it "shouldn't parse entries with too many or too few columns" $ do
      forAll
        (arbitrary `suchThat` \ts -> length ts /= 2)
        (\ts -> parseEntry (T.intercalate "\t" $ map getCsvValue ts) === Nothing)
  describe "formatEntry and parseEntry" $ do
    specify "parseEntry . formatEntry == id" $ do
      property $ \e -> parseEntry (formatEntry e) === Just e

  describe "formatEntries and parseEntries" $ do
    it "parseEntries . formatEntries" $
      property $ \e1 e2 -> parseEntries (formatEntries [e1, e2]) === Just [e1, e2]
