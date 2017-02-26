module Main where

import Options.Applicative
import Data.Semigroup
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Types
import Review
import Database
import System.IO
import System.IO.Error
import Data.Time
import Control.Monad.Catch
import Control.Applicative
import Prompt

type Config = [FilePath]

config :: Parser Config
config = some (argument str (metavar "FILES..."))

options :: ParserInfo Config
options =
  info (helper <*> config) $ mconcat
  [ fullDesc
  , progDesc "Read vocabulary from FILES and start learning session."
  , header "hmemo - CLI spaced repetition learning" ]

iterateUntilNothingM :: Monad m => (a -> m (Maybe a)) -> a -> m [a]
iterateUntilNothingM f x = do
  x' <- f x
  case x' of
    Nothing -> return [x]
    (Just x'') -> (x :) <$> iterateUntilNothingM f x''

main :: IO ()
main = do
  db <- execParser options >>= readDatabase

  hSetBuffering stdout NoBuffering
  hSetBuffering stdin (BlockBuffering Nothing)

  reviewedEntries <- reviewUntilEof db
  let
    weakCards :: [Card]
    weakCards =
      map card $
      filter ((< 4) . head . responseQualities . stats) $
      reviewedEntries
  T.putStrLn "Now reviewing mistakes"
  T.putStrLn ""
  reviewUntil4 weakCards
  T.putStrLn $ inGreen "Done!"
