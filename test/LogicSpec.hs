module LogicSpec where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Instances
import Logic
import Data.Time
import Types

main = hspec spec

spec :: Spec
spec = do
  describe "needsLearning" $ do
    it "should work for cards without stats" $ do
      property $ \t -> needsLearning t (History Nothing []) === True
    it "should work for cards with stats" $ do
      let
        t1 =
          UTCTime
          (fromGregorian 2000 2 3)
          (timeOfDayToTime $ TimeOfDay 18 0 0)
        -- needs review 6 days after t1
        cs = History (Just t1) [4, 4]

        t2 =
          UTCTime
          (fromGregorian 2000 2 8)
          (timeOfDayToTime $ TimeOfDay 18 0 0)
      needsLearning t2 cs `shouldBe` False

      let
        -- not 6 days later, but whithin tolerance
        t3 =
          UTCTime
          (fromGregorian 2000 2 9)
          (timeOfDayToTime $ TimeOfDay 8 0 0)

      needsLearning t3 cs `shouldBe` True

      let
        t4 =
          UTCTime
          (fromGregorian 2000 4 9)
          (timeOfDayToTime $ TimeOfDay 8 23 23)
      needsLearning t4 cs `shouldBe` True
