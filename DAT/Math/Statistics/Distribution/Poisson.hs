module DAT.Math.Statistics.Distribution.Poisson where 

import qualified DAT.Math.Statistics.Distribution as Dist
import Math.Combinatorics.Exact.Factorial

data PoissonDistribution = PoissonDist Double deriving (Show, Eq)

instance Dist.Distribution PoissonDistribution where
  cummulative (PoissonDist lambda) x
    | x' < 0 = 0
    | isInfinite x = 1
    | isNaN x = error $ errMsg "x is NaN"
    | otherwise = sum' lambda x
      where
        sum' l k
          | k == 0 = lambda
          | otherwise = ((exp 1) ** (- l)) * ((l ** k)/ (fact' k) + (sum' l (k - 1)))
          -- | otherwise =  fact' k
        x' = floor x :: Int
        fact' n
          | n == 0 = 1
          | n == 1 = 1
          | otherwise = n * (fact' $ n - 1)

errMsg :: String -> String
errMsg str = "DAT.Math.Statistics.Distribution.Poisson: " ++ str