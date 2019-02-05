module DAT.Math.Statistics.Distribution.Binomial where 

import qualified DAT.Math.Statistics.Distribution as Dist
import Math.Combinatorics.Exact.Binomial (choose)

data BinomialDistribution = Imp | BinomialDist Int Double deriving (Show, Eq)

instance Dist.Distribution BinomialDistribution where
  cummulative Imp _ = 0
  cummulative (BinomialDist n p) x
    | x' < 0 = 0
    | x' >= n = 1
    | isInfinite x = if x' > 0 then 1 else 0
    | isNaN x = error $ errMsg "x is NaN"
    | otherwise = sum' x' n p
      where
        sum' k n p
          | k == 0 = 0
          | otherwise = (pmfB k n p) + (sum' (k - 1) n p)
        x' = floor x :: Int

instance Dist.DiscreteDistribution BinomialDistribution where
  prob Imp _ = 0
  prob (BinomialDist n p) k
    | k < 1 = 0
    | otherwise = pmfB k n p

errMsg :: String -> String
errMsg str = "DAT.Math.Statistics.Distribution.Binomial: " ++ str

pmfB :: Int -> Int -> Double -> Double
pmfB k n p = 
  (fromIntegral (n `choose` k)) * (p ^ k) * ((1 - p) ^ (n - k))