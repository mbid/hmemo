module Serialize where

import Types
import qualified Data.Text.IO as T
import qualified Data.Text as T
import Control.Monad.Catch
import Data.Typeable
import Data.Time.Format
import Data.Time.Clock
import Data.Maybe
import Data.Char
import Control.Monad
import Data.Monoid

data DeserializeException = WrongColumnNumber
                          | InvalidDateString
                          | InvalidResponseQualities
  deriving (Show, Eq, Typeable)
instance Exception DeserializeException where

dateFormat = iso8601DateFormat (Just "%H:%M:%S")

formatTime' :: UTCTime -> T.Text
formatTime' = T.pack . formatTime defaultTimeLocale dateFormat

parseTime' :: T.Text -> Maybe UTCTime
parseTime' = parseTimeM True defaultTimeLocale dateFormat . T.unpack

parseQuality :: MonadThrow m => Char -> m Int
parseQuality c = do
  when (not $ isDigit c) $ throwM InvalidResponseQualities
  let c' = digitToInt c
  when (c' > 5) $ throwM InvalidResponseQualities
  return c'

formatQuality :: Int -> Char
formatQuality = intToDigit

parseQualities :: MonadThrow m => T.Text -> m [Int]
parseQualities = fmap reverse . mapM parseQuality . T.unpack

formatQualities :: [Int] -> T.Text
formatQualities = T.pack . reverse . map formatQuality

parseEntry :: MonadThrow m => T.Text -> m CH
parseEntry l = do
  let components = map T.strip $ T.splitOn "\t" l
  case components of
    [front, back] -> return (Card { back = back, front = front}, History Nothing [])
    [front, back, lr, qs] -> do
      lr' <- fromMaybe (throwM InvalidDateString) $ return <$> parseTime' lr
      qs' <- parseQualities qs
      return (Card { back = back, front = front}, History (Just lr') qs')
    _ -> throwM WrongColumnNumber

formatEntry :: CH -> T.Text
formatEntry (c, cs) = T.intercalate "\t" $
  [front c, back c]
  ++
  case lastReview cs of
    Just lr -> [formatTime' lr, formatQualities $ qualities cs]
    Nothing -> []

parseEntries :: MonadThrow m => T.Text -> m [CH]
parseEntries = mapM parseEntry . T.lines

formatEntries :: [CH] -> T.Text
formatEntries = T.concat . map ((<> "\n") . formatEntry)

readEntries :: FilePath -> IO [CH]
readEntries = T.readFile >=> parseEntries

writeEntries :: FilePath -> [CH] -> IO ()
writeEntries fp = T.writeFile fp . formatEntries
