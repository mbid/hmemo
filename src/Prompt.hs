module Prompt where

import Types
import Database
import Review
import Serialize
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Control.Monad.Loops
import Data.Monoid
import Data.Time
import Data.Maybe
import Control.Monad.Catch
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class
import System.IO.Error
import Control.Exception (assert)

firstColumn = [ "Front: "
              , "Back: "
              , "Yes!"
              , "No: "
              , "Rating (0 to 5): " ]

justifyFirstColumn :: T.Text -> T.Text
justifyFirstColumn t =
  assert (t `elem` firstColumn) $ 
  T.justifyLeft (maximum $ map T.length firstColumn) ' ' t

inRed :: T.Text -> T.Text
inRed t = "\x1b[31m" <> t <> "\x1b[0m"
inGreen :: T.Text -> T.Text
inGreen t = "\x1b[32m" <> t <> "\x1b[0m"

prompt :: T.Text -> IO T.Text
prompt t = do
  T.putStr $ justifyFirstColumn t
  T.strip <$> T.getLine

testCard :: Card -> IO Bool
testCard card = do
  T.putStrLn $ justifyFirstColumn "Front: " <> front card
  response <- prompt "Back: "
  if response == back card
    then do
      T.putStrLn $ justifyFirstColumn $ inGreen "Yes!"
      return True
    else do
      T.putStrLn $ inRed $ justifyFirstColumn "No: " <> back card
      return False

parseQuality' :: T.Text -> Maybe Int
parseQuality' q = case T.unpack q of
  [q'] -> parseQuality q'
  _ -> Nothing

evaluate :: IO Int
evaluate = do
  T.putStr "Rating (0 to 5): "
  q <- T.getLine
  fromMaybe evaluate (return <$> parseQuality' q)

review :: Card -> IO Int
review c = testCard c >> evaluate

reviewUntil4 :: [Card] -> IO ()
reviewUntil4 [] = return ()
reviewUntil4 cs = do
  qs <- mapM review cs
  reviewUntil4 $ map snd $ filter ((< 4) . fst) $ zip qs cs

updateStats :: UTCTime -> Int -> CardStats -> CardStats
updateStats t q cs = CardStats { lastReview = (Just t)
                               , responseQualities = q : responseQualities cs }

reviewSaveAndUpdate :: Database -> IO (Maybe (Entry, Database))
reviewSaveAndUpdate db = runMaybeT $ do
  t <- lift $ getCurrentTime
  (fp, (c, cs)) <- MaybeT $ return $ listToMaybe $ reviewables t db
  q <- lift $ review c
  t <- lift $ getCurrentTime
  let 
    cs' = updateStats t q cs
    db' = setStats fp c cs' db
  lift $ savePart fp db'
  return ((c, cs'), db')

reviewUntilEof :: Database -> IO [Entry]
reviewUntilEof =
  unfoldrM 
  (handleIf isEOFError (const $ putStrLn "" >> return Nothing) .
  reviewSaveAndUpdate)
