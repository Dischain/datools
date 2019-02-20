module DAT.Math.Statistics.Correlation.Pearson
(
  pearson
) where

import DAT.Row
import DAT.Math.Statistics

pearson :: Row Double -> Row Double -> Double
pearson r1 r2
  | lengthR r1 /= lengthR r2 = error $ errMsg "samples should be of equal length"
  | otherwise = sampleCovariance r1 r2 / (stdDev1 * stdDev2)
    where
      stdDev1 = stdDev r1
      stdDev2 = stdDev r2
 
errMsg :: String -> String
errMsg str = "DAT.Math.Statistics.Correlation.Pearson: " ++ str