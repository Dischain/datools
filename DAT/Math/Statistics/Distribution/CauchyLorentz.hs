module DAT.Math.Statistics.Distribution.CauchyLorentz where

import qualified DAT.Math.Statistics.Distribution as Dist

data CauchyLorentz = CauchyLorentz {
  x0 :: Double,  -- | location
  gamma :: Double -- | scale
} deriving (Eq, Show)

instance Dist.Distribution CauchyLorentz where
  cummulative (CauchyLorentz x0 gamma) x = 0.5 + atan( (x - x0) / gamma ) / pi

instance Dist.ContinuousDistribution CauchyLorentz where
  density (CauchyLorentz x0 gamma) x = 
    1 / (pi * gamma * (1 + ((x - x0) / gamma) ** 2))