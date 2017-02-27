module Main where

import Options.Applicative
import Data.Semigroup
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Types
import Logic
import Database
import System.IO
import System.IO.Error
import Data.Time
import Control.Monad.Catch
import Control.Applicative
import Prompt
import Control.Monad

type Config = [Deck]

config :: Parser Config
config = some (argument str (metavar "FILES..."))

options :: ParserInfo Config
options =
  info (helper <*> config) $ mconcat
  [ fullDesc
  , progDesc "Read vocabulary from FILES and start learning session."
  , header "hmemo - CLI spaced repetition learning" ]

main :: IO ()
main = do
  db <- execParser options >>= readDatabase

  hSetBuffering stdout NoBuffering
  hSetBuffering stdin (BlockBuffering Nothing)

  reviewedEntries <- reviewUntilEof db
  clear

  let
    weakCards :: [Card]
    weakCards =
      map card $
      filter ((< 4) . head . qualities . history) $
      reviewedEntries
  when (not $ null weakCards) $ do
    T.putStrLn $ 
         "Now reviewing "
      <> (T.pack $ show $ length weakCards)
      <> (if (length weakCards == 1) then " mistake" else " mistakes")
    T.putStrLn "Press enter to continue"
    T.getLine
    reviewUntil4 weakCards
    clear
  T.putStrLn $ inGreen "Done!"
