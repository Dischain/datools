module DAT.Math.Statistics.Correlation.Kendall
(
  kendall,
  kendallSort,
  numOrdered
) where

import DAT.Row
import DAT.Table
import DAT.Math.Statistics.Test.Rank
import DAT.Math.Matrix

-- | Kendall - tau correlation test
kendall :: Row Rank -> Row Rank -> Double
kendall r1 r2 
  | lengthR r1 /= lengthR r2 = error $ errMsg "samples should be of equal length"
  | otherwise = 2 * (nc - nd) / (n * (n - 1))
    where
      r = kendallSort r1 r2
      nc = fromIntegral $ numOrdered r concorded
      nd = fromIntegral $ numOrdered r discorded
      concorded a b = a < b
      discorded a b = not (a < b)
      n = fromIntegral $ lengthR r1

numOrdered :: Row Rank -> (Rank -> Rank -> Bool) -> Int
numOrdered (Row []) ord = 0
numOrdered r@(Row rs) ord = sum' rs
  where
    sum' :: [Rank] -> Int
    sum' (x:xs) = (count' x xs 0) + (sum' xs)
    sum' [] = 0

    count' :: Rank -> [Rank] -> Int -> Int
    count' a (x:xs@(_:_)) acc 
      | ord a x = count' a xs (acc + 1)
      | otherwise = count' a xs acc
    count' a [x] acc
      | ord a x = acc + 1
      | otherwise = acc
    count' a [] acc = acc

kendallSort :: Row Rank -> Row Rank -> Row Rank
kendallSort r1@(Row xs) r2@(Row ys) = 
  (ithRow' 1) . transpose' . (sortT 0) . transpose' $ (ConsT r1 (ConsT r2 Empty))

errMsg :: String -> String
errMsg str = "DAT.Math.Statistics.Correlation.Kendall: " ++ str