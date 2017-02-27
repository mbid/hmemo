module Logic where

import SuperMemo2
import Data.Ratio
import Types
import Database
import Data.Time
import Data.Maybe
import qualified Data.Map.Lazy as M
import Data.Monoid

nextReview :: History -> Maybe UTCTime
nextReview cs = do
  lr <- lastReview cs
  return $ lr {utctDay = addDays days $ utctDay lr}
  where
    days = fromIntegral $ head $ intervals $ qualities cs

halfDay :: NominalDiffTime
halfDay = 3600 * 12

approxNotBefore :: UTCTime -> UTCTime -> Bool
approxNotBefore s t = diffUTCTime s t >= -halfDay

needsLearning :: UTCTime -> History -> Bool
needsLearning now cs = fromMaybe True $ (now `approxNotBefore`) <$> nextReview cs

learnables :: UTCTime -> Database -> [DCH]
learnables now = filter (needsLearning now . history . snd) . flattenDatabase

qualityHistogram :: Database -> M.Map (Maybe Int) Int
qualityHistogram db =
  M.fromList $
  map (\q -> (q, count q)) $
  map Just [0 .. 5] <> [Nothing]
  where
    qs :: [Maybe Int]
    qs = map (listToMaybe . qualities. history . ch) $ flattenDatabase db
    count :: Maybe Int -> Int
    count q = length $ filter (== q) qs
