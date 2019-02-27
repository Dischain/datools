module DAT.Math.Statistics.Regression.Polynomial
(
  polynomial
) where

import DAT.Row
import DAT.Table
import DAT.Math.Matrix
import DAT.Math.MatrixOps

polynomial :: Int -> Row Double -> Row Double -> Row Double
polynomial degree vars depvars
  | degree >= n = error $ errMsg "number of degrees should not exceed the number of input variables"
  | otherwise = toRow (inv `mul` (xsTransposed `mul` ys))
    where
      xsTransposed = transpose' xs
      xs = mkMatrix degree vars
      inv = inverse (xsTransposed `mul` xs)
      ys = toColumn depvars
      n = fromIntegral $ lengthR vars

mkMatrix :: Int -> Row Double -> Table Double
mkMatrix degree (Row (x : xs)) =
  ConsT (Row $ map (\a -> x ** (fromIntegral a)) ns) (mkMatrix degree (Row xs))
    where ns = [0 .. degree]
mkMatrix _ (Row []) = Empty

errMsg :: String -> String
errMsg str = "DAT.Math.Statistics.Regression.Polynomial: " ++ str
