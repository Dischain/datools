module DAT.Math.Statistics.Test.MannWhitney 
( 
  MannWhitneyRes (..),
  mannWhitney
) where

import DAT.Math.Statistics.Test.Rank hiding (errMsg)
import DAT.Row 
import DAT.Table
import Data.Hashable

data MannWhitneyRes = MannWhitneyRes {
  n1 :: Int,
  n2 :: Int,
  u :: Double
} deriving (Show, Eq)

mannWhitney :: (Eq a, Ord a, Fractional a, Hashable a) => 
                Table a -> (a -> a -> Bool) -> MannWhitneyRes
mannWhitney t eq
  | numRows t /= 2 = error $ errMsg "should be provided only two samples"
  | otherwise = MannWhitneyRes { n1 = ni 0, n2 = ni 1, u = compU mwtest }
    where
      rankedSampleWith_ni :: Table Rank -> [(Row Rank, Int)]
      rankedSampleWith_ni (ConsT r rs) = [(r, lengthR r)] ++ rankedSampleWith_ni rs
      rankedSampleWith_ni Empty = id []

      rankedS = rankedSample t eq

      mwtest = map (\(ri, li) -> 
        toDouble (foldl (+) r0 ri) - fromIntegral li * (fromIntegral li + 1) / 2 ) (rankedSampleWith_ni rankedS)

      ni i = case ithRow i t of 
        (Just (ConsT r Empty)) -> lengthR r
        _ -> 0

compU :: [Double] -> Double
compU [r1, r2]
  | r1 < r2 = r1
  | otherwise = r2

errMsg :: String -> String
errMsg msg = "DAT.Math.Statistics.Test.MannWhitney: " ++ msg
