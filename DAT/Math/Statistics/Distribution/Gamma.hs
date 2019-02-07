module DAT.Math.Statistics.Distribution.Gamma where 

import qualified DAT.Math.Statistics.Distribution as Dist
import Math.Gamma.Incomplete
import qualified Math.Gamma as G

data Gamma = GammaDist Double Double deriving (Eq, Show)

instance Dist.Distribution Gamma where
  cummulative (GammaDist a b) k 
    | k <= 0    = 0
    | otherwise = (1 / (G.gamma a)) * (lowerGammaHypGeom a (b * k))

instance Dist.ContinuousDistribution Gamma where
  density (GammaDist a b) k
    | a <= 0 || b <= 0 = error $ errMsg "shape or rate parameter should be greater then 0"
    | k <= 0 = 0
    | otherwise = (b ** a) / (G.gamma a) * (k ** (a - 1) * (exp 1) ** (- (b * k)))

errMsg :: String -> String
errMsg str = "DAT.Math.Statistics.Distribution.Gamma: " ++ str