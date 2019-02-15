module DAT.Math.Statistics.Test.MannWhitney 
( 
  MannWhitneyRes (..),
  mannWhitney
) where

import DAT.Math.Statistics.Test.Rank
import DAT.Row hiding (fromList)
import DAT.Table
import Data.HashMap hiding (toList)
import Data.Hashable

data MannWhitneyRes = MannWhitneyRes {
  n1 :: Int,
  n2 :: Int,
  u :: Double
} deriving (Show, Eq)

mannWhitney :: (Eq a, Ord a, Fractional a, Hashable a) => 
                Table a -> MannWhitneyRes
mannWhitney t
  | numRows t /= 2 = error $ errMsg "should be provided only two samples"
  | otherwise = MannWhitneyRes { n1 = ni 0, n2 = ni 1, u = compU mwtest }
    where
      rankedSampleWith_ni :: Table Rank -> [(Row Rank, Int)]
      rankedSampleWith_ni (ConsT r rs) = [(r, lengthR r)] ++ rankedSampleWith_ni rs
      rankedSampleWith_ni Empty = id []

      rankedS = rankedSample t

      mwtest = fmap (\(ri, li) -> 
        toDouble (foldl (+) r0 ri) - fromIntegral li * (fromIntegral li + 1) / 2 ) (rankedSampleWith_ni rankedS)

      ni i = case ithRow i t of 
        (Just (ConsT r Empty)) -> lengthR r
        _ -> 0

compU :: [Double] -> Double
compU [r1, r2]
  | r1 < r2 = r1
  | otherwise = r2

rankedSample :: (Eq a, Ord a, Fractional a, Hashable a) => 
                Table a -> Table Rank
rankedSample t = 
  mapRows (\r -> fmap (\a -> rm ! a) r) t
    where
      rm = rankedMap t

rankedMap :: (Eq a, Ord a, Fractional a, Hashable a) => 
              Table a -> Map a Rank
rankedMap t = fromList $ toList (zipR asSortedRow (fractionalRank asSortedRow))
  where
    asSortedRow = sortR $ toRow t

errMsg :: String -> String
errMsg msg = "DAT.Math.Statistics.Test.MannWhitney: " ++ msg
