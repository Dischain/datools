module DAT.Math.Statistics.Distribution.Poisson where 

import qualified DAT.Math.Statistics.Distribution as Dist

data PoissonDistribution = PoissonDist Double deriving (Show, Eq)

instance Dist.Distribution PoissonDistribution where
  cummulative (PoissonDist lambda) x
    | x' < 0 = 0
    | isInfinite x = 1
    | isNaN x = error $ errMsg "x is NaN"
    | otherwise = e * (sum' lambda x')
      where
        sum' l k
          | k == 0 = 0
          | otherwise = (l ** (fromIntegral k) / fact' (fromIntegral k)) + sum' l (k - 1)
        x' = floor x :: Int
        e = (exp 1 ) ** (- lambda)

instance Dist.DiscreteDistribution PoissonDistribution where
  prob (PoissonDist lambda) k
    | k < 1 = 0
    | otherwise = ((lambda ** (fromIntegral k)) * e) / (fromIntegral $ fact' k)
      where
        e = (exp 1 ) ** (- lambda)

fact' n
  | n == 0 = 1
  | n == 1 = 1
  | otherwise = n * (fact' $ n - 1)
  
errMsg :: String -> String
errMsg str = "DAT.Math.Statistics.Distribution.Poisson: " ++ str