module DAT.Math.Statistics.Distribution.DiscreteUniform where 

import qualified DAT.Math.Statistics.Distribution as Dist

data DiscreteUniform = Uniform Int Int deriving (Eq, Show)

instance Dist.Distribution DiscreteUniform where
  cummulative (Uniform a b) x
    | x > fromIntegral b = 1
    | x < fromIntegral a = 0
    | otherwise = fromIntegral (floor x - a + 1) / fromIntegral (b - a + 1)

instance Dist.DiscreteDistribution DiscreteUniform where
  prob (Uniform a b) k
    | k >= a && k <= b = 1 / fromIntegral (b - a + 1)
    | otherwise = 0

errMsg :: String -> String
errMsg str = "DAT.Math.Statistics.Distribution.DiscreteUniform: " ++ str