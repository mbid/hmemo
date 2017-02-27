module Prompt where

import Types
import Database
import Logic
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
              , "Yes! "
              , "No. "
              , "Repeat: "
              , "Rating: " ]

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
      T.putStrLn $ inGreen $ justifyFirstColumn "Yes! " <> back card
      return True
    else do
      (return ()) `untilM_` do
        T.putStrLn $ inRed $ justifyFirstColumn "No. " <> back card
        response <- prompt "Repeat: "
        return $ response == back card
      return False

parseQuality' :: T.Text -> Maybe Int
parseQuality' q = case T.unpack q of
  [q'] -> parseQuality q'
  _ -> Nothing

evaluate :: IO Int
evaluate = do
  q <- prompt "Rating: "
  fromMaybe evaluate (return <$> parseQuality' q)

clear :: IO ()
clear = T.putStr "\x1b[2J\x1b[H"

review :: Card -> IO Int
review c = do
  clear
  testCard c
  q <- evaluate
  return q

reviewUntil4 :: [Card] -> IO ()
reviewUntil4 [] = return ()
reviewUntil4 cs = do
  qs <- mapM review cs
  reviewUntil4 $ map snd $ filter ((< 4) . fst) $ zip qs cs

updateStats :: UTCTime -> Int -> History -> History
updateStats t q cs = History { lastReview = (Just t)
                             , qualities = q : qualities cs }

reviewSaveAndUpdate :: Database -> IO (Maybe (CH, Database))
reviewSaveAndUpdate db = runMaybeT $ do
  t <- lift $ getCurrentTime
  (d, (c, cs)) <- MaybeT $ return $ listToMaybe $ learnables t db
  q <- lift $ review c
  t <- lift $ getCurrentTime
  let 
    cs' = updateStats t q cs
    db' = setStats d c cs' db
  lift $ saveDeck d db'
  return ((c, cs'), db')

reviewUntilEof :: Database -> IO [CH]
reviewUntilEof =
  unfoldrM 
  (handleIf isEOFError (const $ putStr "\n\n" >> return Nothing) .
  reviewSaveAndUpdate)
