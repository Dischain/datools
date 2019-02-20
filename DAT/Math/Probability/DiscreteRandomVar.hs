module DAT.Math.Probability.DiscreteRandomVar 
(
  DRV (..),
  mkDRV,
  numProbs
) where

import DAT.Row

-- | Discrete random variable
data DRV = DRV { 
  vals :: Row Double,
  probs :: Row Double 
} deriving (Show)

mkDRV :: Row Double -> Row Double -> DRV
mkDRV r1 r2
  | lengthR r1 /= lengthR r2 = error $ errMsg "incorrect sizes of values and probabilities"
  | foldl (+) 0 r2 /= 1 = error $ errMsg "incorrect probabilities distribution"
  | otherwise = DRV { vals = r1, probs = r2 }

numProbs :: DRV -> Int
numProbs (DRV vals _) = lengthR vals

errMsg :: String -> String
errMsg str = "DAT.Math.Probability.DiscreteRandomVar: " ++ str
  