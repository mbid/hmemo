module Database where

import Types
import Serialize
import Data.List
import Data.Maybe

type Database = [(Deck, [CH])]

readDatabase :: [Deck] -> IO Database
readDatabase fps = zip fps <$> mapM readEntries fps

saveDeck :: Deck -> Database -> IO ()
saveDeck fp db = writeEntries fp . snd . fromJust . find ((== fp) . fst) $ db

flattenDatabase :: Database -> [DCH]
flattenDatabase = concat . map (\(fp, es) -> map (fp,) es)

adjust :: Eq a => (b -> b) -> a -> [(a, b)] -> [(a, b)]
adjust f k = map f'
  where
    --f' :: (a, b) -> (a, b)
    f' (k', x) | k' == k = (k', f x)
               | otherwise = (k', x)

setStats :: Deck -> Card -> History -> Database -> Database
setStats fp c cs = adjust (adjust (const cs) c) fp
