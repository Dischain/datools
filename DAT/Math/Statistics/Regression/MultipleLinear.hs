module DAT.Math.Statistics.Regression.MultipleLinear
(
  multipleLinear,
  mkMatrix
) where

import DAT.Row
import DAT.Table
import DAT.Table.Misc
import DAT.Math.Matrix
import DAT.Math.MatrixOps

-- | Each given matrix row are independent variables
multipleLinear :: [[Double]] -> Row Double -> Row Double
multipleLinear vars depvars
  | length (vars !! 0) /= length depvars = error $ errMsg "number of independent variables "
      ++ "should not exceed the number of dependent variables"
  | otherwise = toRow (inv `mul` (xsTransposed `mul` ys))
    where
      xsTransposed = transpose' xs
      xs = mkMatrix vars
      inv = inverse (xsTransposed `mul` xs)
      ys = toColumn depvars

mkMatrix :: [[Double]] -> Table Double
mkMatrix vars = mkTable $ (replicate n 1) : vars
  where 
    n = length (vars !! 0)

errMsg :: String -> String
errMsg str = "DAT.Math.Statistics.Regression.MultipleLinear: " ++ str