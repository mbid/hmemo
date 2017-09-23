module Main where

import Options.Applicative
import Data.Monoid
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
import Data.Maybe

import qualified Data.Map.Lazy as M

type Config = [Deck]

config :: Parser Config
config = some (argument str (metavar "FILES..."))

options :: ParserInfo Config
options =
  info (helper <*> config) $ mconcat
  [ fullDesc
  , progDesc "Read vocabulary from FILES and start learning session."
  , header "hmemo - CLI spaced repetition learning" ]

printTable :: [[T.Text]] -> IO ()
printTable = mapM_ $ T.putStrLn . T.intercalate " "

wait :: IO ()
wait = do
  T.putStrLn ""
  T.putStrLn "Press enter to continue."
  T.getLine
  return ()

main :: IO ()
main = handleIf isEOFError (const $ putStrLn "") $ do
  db <- execParser options >>= readDatabase
  let
    hist = qualityHistogram db
    cardNumber = sum $ map snd $ M.toList hist
    rememberedNumber = (hist M.! Just 4) + (hist M.! Just 5)
    newNumber = hist M.! Nothing
  now <- getCurrentTime
  let
    ls = learnables now db
    learnableNumber = length ls
    reviewableNumber = length $ filter (isJust . lastReview . history . ch) ls

  printTable
    [ ["Total number of words:", T.pack $ show cardNumber]
    , ["Remembered words:", T.pack $ show rememberedNumber]
    , ["New words:", T.pack $ show newNumber] ]

  putStrLn ""

  when (learnableNumber == 0) $ do
    putStrLn "There are no words to learn or review right now."

  when (learnableNumber /= 0) $ do
    hSetBuffering stdout NoBuffering
    hSetBuffering stdin (BlockBuffering Nothing)

    putStrLn $
      case (reviewableNumber, newNumber) of
        (0, 1) -> "There is 1 new word."
        (0, _) -> "There are " <> show newNumber <> " new words."
        (_, 1) -> "1 word needs to be reviewed."
        (_, 0) -> show reviewableNumber <> " words need to be reviewed."
        (1, 1) -> "1 word needs to be reviewed, and there is 1 new word."
        (_, _) -> 
          show reviewableNumber <>
          " words need to be reviewed, and there are " <>
          show newNumber <>
          " new words."
    putStrLn "Press Ctrl-d to stop at any time."

    wait

    reviewedEntries <- reviewUntilEof db

    let
      weakCards :: [Card]
      weakCards =
        map card $
        filter ((< 4) . head . qualities . history) $
        reviewedEntries
    when (not $ null weakCards) $ do
      clear
      T.putStrLn $ 
           "Now reviewing "
        <> (T.pack $ show $ length weakCards)
        <> (if (length weakCards == 1) then " mistake." else " mistakes.")
      wait
      reviewUntil4 weakCards

    T.putStrLn ""
    T.putStrLn $ inGreen "Done!"
