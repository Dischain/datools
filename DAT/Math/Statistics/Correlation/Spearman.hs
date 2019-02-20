module DAT.Math.Statistics.Correlation.Spearman
(
  spearman
) where

import DAT.Math.Statistics.Test.Rank
import DAT.Row hiding (fromList)
import DAT.Table
import Data.HashMap hiding (toList)
import Data.Hashable

spearman :: (Eq a, Ord a, Fractional a, Hashable a) => 
            Row a -> Row a -> Double
spearman r1 r2
  | lengthR r1 /= lengthR r2 = error $ errMsg "samples should be of equal length"
  | otherwise = 1 - (6 * d2) / (n * (n ** 2 - 1))
    where        
      d2 = foldl (+) 0 
                 (fmap (\(Rank r) -> r ** 2) ((-) <$> (rankedRow r1) <*> (rankedRow r2)))      

      n = fromIntegral $ lengthR r1

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