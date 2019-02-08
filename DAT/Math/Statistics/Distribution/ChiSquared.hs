module DAT.Math.Statistics.Distribution.ChiSquared where 

import qualified DAT.Math.Statistics.Distribution as Dist
import Math.Gamma.Incomplete
import qualified Math.Gamma as G

-- | Continous distribution with k degrees of freedom which is a distribution
-- of a sum of squares of k independent standart normal variables. Is a special case
-- of the gamma distribution.
data ChiSquared = ChiSquared Int deriving (Eq, Show)

instance Dist.Distribution ChiSquared where
  cummulative (ChiSquared k) x
    | x <= 0 = 0
    | otherwise = (1 / G.gamma (ndf / 2)) * (lowerGammaHypGeom (ndf / 2) (x / 2))
      where 
        ndf = fromIntegral k

instance Dist.ContinuousDistribution ChiSquared where
  density (ChiSquared k) x
    | x <= 0 = 0
    | otherwise = 
      (1 / (2 ** (ndf / 2) * G.gamma (ndf / 2))) * (x ** (ndf / 2 - 1) * (exp 1) ** ((- 1) * x / 2))
      where 
        ndf = fromIntegral k