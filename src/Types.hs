module Types where

import Data.Time
import qualified Data.Text as T

data Card = Card { front :: T.Text
                 , back :: T.Text }
  deriving (Show, Eq)

data History = History { lastReview :: Maybe UTCTime
                       , qualities :: [Int] }
  deriving (Show, Eq)

type CH = (Card, History)
card :: CH -> Card
card = fst
history :: CH -> History
history = snd

type Deck = FilePath
type DCH = (Deck, CH)
deck :: DCH -> Deck
deck = fst
ch :: DCH -> CH
ch = snd
