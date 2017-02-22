module SuperMemo2 where

--nextEFactor :: Int -> Float -> Float
nextEFactor :: (Ord a, Fractional a) => Int -> a -> a
nextEFactor q ef = 
  max 1.3 $
  ef - 0.8 + 0.28 * fromIntegral q - 0.02 * (fromIntegral q) ^ 2
  --ef + 0.1 - (5 - fromIntegral q) * (0.08 + (5 - fromIntegral q) * 0.02)

--eFactors :: [Int] -> [Float]
eFactors :: (Ord a, Fractional a) => [Int] -> [a]
eFactors = scanr nextEFactor 2.5

intervals :: [Int] -> [Int]
intervals qualities =
  map fst $
  scanr nextInterval (0, True) $
  zip qualities (eFactors qualities)
  where
    nextInterval :: (Int, Float) -> (Int, Bool) -> (Int, Bool)
    nextInterval (q, ef) (0, True) = (1, True)
    nextInterval (q, ef) (1, True)
      | q >= 3 = (6, False)
      | otherwise = (1, True)
    nextInterval (q, ef) (n, False)
      | q < 3 = (1, True)
      | otherwise = (ceiling $ fromIntegral n * ef, False)
