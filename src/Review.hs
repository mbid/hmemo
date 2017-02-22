module Review where

import SuperMemo2
import Data.Ratio
import Types
import Database
import Data.Time
import Data.Maybe

nextReview :: CardStats -> Maybe UTCTime
nextReview cs = do
  lr <- lastReview cs
  return $ lr {utctDay = addDays days $ utctDay lr}
  where
    days = fromIntegral $ head $ intervals $ responseQualities cs

halfDay :: NominalDiffTime
halfDay = 3600 * 12

approxNotBefore :: UTCTime -> UTCTime -> Bool
approxNotBefore s t = diffUTCTime s t >= -halfDay

needsReview :: UTCTime -> CardStats -> Bool
needsReview now cs = fromMaybe True $ (now `approxNotBefore`) <$> nextReview cs

reviewables :: UTCTime -> Database -> [(FilePath, Entry)]
reviewables now = filter (needsReview now . stats . snd) . flattenDatabase
