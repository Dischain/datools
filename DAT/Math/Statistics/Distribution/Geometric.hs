module DAT.Math.Statistics.Distribution.Geometric where 

import qualified DAT.Math.Statistics.Distribution as Dist

-- | Models the number of failures before one success in a series of independent
-- trials, where each trial results in either success or failure, and probability
-- of success in any individual trial is constant
data GeometricDistribution = Imp | GeometricDist Double deriving (Show, Eq)

instance Dist.Distribution GeometricDistribution where
  cummulative Imp _ = 0
  cummulative (GeometricDist p) x
    | x' < 1 = 0
    | isInfinite x = 1
    | isNaN x = error $ errMsg "x is NaN"
    | otherwise = 1 - (1 - p) ^ x'
      where
        x' = floor x :: Int

instance Dist.DiscreteDistribution GeometricDistribution where
  prob Imp _ = 0
  prob (GeometricDist p) k
    | k < 1 = 0
    | otherwise = (1 - p) ^ (k - 1) * p

errMsg :: String -> String
errMsg str = "DAT.Math.Statistics.Distribution.Geometric: " ++ str