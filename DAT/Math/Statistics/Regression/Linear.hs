module DAT.Math.Statistics.Regression.Linear
(
  linearRegr
) where

import DAT.Row
import DAT.Math.Statistics

linearRegr :: Row Double -> Row Double -> (Double, Double)
linearRegr r1 r2
  | lengthR r1 /= lengthR r2 = error $ errMsg "samples should be of equal length"
  | otherwise = (a, b)
    where      
      b = sampleCovariance r1 r2 / sampleVariance r1
      a = mean r2 - (mean r1) * b

errMsg :: String -> String
errMsg str = "DAT.Math.Statistics.Regression.Linear: " ++ str