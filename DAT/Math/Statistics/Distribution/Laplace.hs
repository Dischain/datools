module DAT.Math.Statistics.Distribution.Laplace where

import qualified DAT.Math.Statistics.Distribution as Dist

data Laplace = Laplace {
  nu :: Double,  -- | location
  b :: Double -- | scale
} deriving (Eq, Show)

instance Dist.Distribution Laplace where
  cummulative (Laplace nu b) x
    | x <= nu = 0.5 * (exp (x - nu) / b)
    | otherwise = 1 - 0.5 * (exp (- (x - nu) / b))

instance Dist.ContinuousDistribution Laplace where
  density (Laplace nu b) x = 
    (1 / (2 * b)) * exp (- (abs $ x - nu) / b)