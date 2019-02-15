module DAT.Math.Statistics.Correlation.Spearman
(
  spearman
) where

import DAT.Math.Statistics.Test.Rank hiding (rankedSample, rankedMap)
import DAT.Row hiding (fromList)
import DAT.Table
import Data.HashMap hiding (toList)
import Data.Hashable

spearman :: (Eq a, Ord a, Fractional a, Hashable a) => 
            Table a -> Double
spearman t
  | numRows t /= 2 = error $ errMsg "should be provided only two samples"
  | ni 0 /= ni 1 = error $ errMsg "samples should be of equal length"
  | otherwise = 1 - (6 * d2) / (n * (n ** 2 - 1))
    where        
      d2 = foldl (+) 0 (fmap (\(Rank r) -> r ** 2) ((-) <$> r1 <*> r2))
      
      r1 = rankedRow (ri 0)
      r2 = rankedRow (ri 1)

      ri i = case ithRow i t of
        (Just (ConsT r Empty)) -> r
        _ -> Row []

      ni i = lengthR $ ri i

      n = fromIntegral $ ni 0

rankedRow :: (Eq a, Ord a, Fractional a, Hashable a) => 
              Row a -> Row Rank
rankedRow rw = fmap (\a -> rm ! a) rw
  where
    rm = rankedMap rw

rankedMap :: (Eq a, Ord a, Fractional a, Hashable a) => 
              Row a -> Map a Rank
rankedMap t = fromList $ toList (zipR (reverseR asSortedRow) (standartRank asSortedRow))
  where
    asSortedRow = sortR t

errMsg :: String -> String
errMsg msg = "DAT.Math.Statistics.Correlation.Spearman: " ++ msg