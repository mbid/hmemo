module Util where

import Test.QuickCheck
import Test.QuickCheck.Instances
import Data.Time
import Types
import Control.Monad
import qualified Data.Text as T

data CsvValue = CsvValue { getCsvValue :: T.Text }
  deriving (Show, Eq)

isStripped :: T.Text -> Bool
isStripped t = t == T.strip t

instance Arbitrary CsvValue where
  arbitrary = 
    CsvValue <$>
    arbitrary
    `suchThat` (\t -> '\n' `notElem` T.unpack t)
    `suchThat` (\t -> '\t' `notElem` T.unpack t)
    `suchThat` (\t -> t == T.strip t)

  shrink (CsvValue t) = map CsvValue $ shrink t

instance Arbitrary Card where
  arbitrary = liftM2 Card (getCsvValue <$> arbitrary) (getCsvValue <$> arbitrary)
  shrink (Card s t) = [Card s' t' | s' <- shrink s, t' <- shrink t]

floorSecond :: UTCTime -> UTCTime
floorSecond t = t {utctDayTime = secondsToDiffTime $ picos `div` (10 ^ 12)}
  where
    picos = diffTimeToPicoseconds $ utctDayTime t

instance Arbitrary History where
  arbitrary = do
    lr <- fmap floorSecond <$> arbitrary
    qs <- case lr of
      Nothing -> return []
      (Just _) -> listOf1 (elements [0 .. 5])
    return $ History lr qs
