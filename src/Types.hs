module Types where

import Data.Time
import qualified Data.Text as T

data Card = Card { front :: T.Text
                 , back :: T.Text }
  deriving (Show, Eq)

data CardStats = CardStats { lastReview :: Maybe UTCTime
                           , responseQualities :: [Int] }
  deriving (Show, Eq)

type Entry = (Card, CardStats)
card :: Entry -> Card
card = fst
stats :: Entry -> CardStats
stats = snd
