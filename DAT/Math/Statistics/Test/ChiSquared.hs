module DAT.Math.Statistics.Test.ChiSquared where 

import DAT.Math.Statistics.Distribution.ChiSquared
import DAT.Math.Statistics.Distribution
import DAT.Row (Row, lengthR)

chiSquaredTest :: Row (Int, Double) -> Int -> Double
chiSquaredTest r ndf
  | ndf < 0 = error $ errMsg "number of degrees of freedom must be positive"
  | otherwise = cummulative (ChiSquared ndf') chiSquared'
    where 
      ndf' = (lengthR r) - 1 - ndf
      chiSquared' = foldl (\acc (obs, ex) -> acc + (fromIntegral obs - ex) / ex) 0 r

errMsg :: String -> String
errMsg str = "DAT.Math.Statistics.Test.ChiSquared: " ++ str