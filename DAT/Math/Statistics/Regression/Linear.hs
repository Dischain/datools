module DAT.Math.Statistics.Regression.Linear
(
  linearRegr,
  LinearRes (..)
) where

import DAT.Row
import DAT.Math.Statistics

data LinearRes = LinearRes {
  a :: Double,
  b :: Double
} deriving (Show)

linearRegr :: Row Double -> Row Double -> LinearRes
linearRegr r1 r2
  | lengthR r1 /= lengthR r2 = error $ errMsg "samples should be of equal length"
  | otherwise = LinearRes a b
    where      
      b = sampleCovariance r1 r2 / sampleVariance r1
      a = mean r2 - (mean r1) * b

errMsg :: String -> String
errMsg str = "DAT.Math.Statistics.Regression.Linear: " ++ str