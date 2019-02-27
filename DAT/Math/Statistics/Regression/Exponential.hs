module DAT.Math.Statistics.Regression.Exponential
(
  exponential,
  ExponentialRes (..)
) where

import DAT.Row
import DAT.Table

-- | Result of calculation of the exponential regression
-- quotients, formed by equation y = a * e ^ (b * x)
-- where a, b - are the quotients
data ExponentialRes = ExponentialRes {
  a :: Double,
  b :: Double
} deriving (Show)

exponential :: Row Double -> Row Double -> ExponentialRes
exponential r1 r2 
  | lengthR r1 /= lengthR r2 = error $ errMsg "samples should be of equal length"
  | otherwise = ExponentialRes a b
    where
      s_xizi = foldl (+) 0 ((*) <$> r1 <*> (fmap log r2))
      s_xi = foldl (+) 0 r1
      s_zi = foldl (+) 0 (fmap log r2)
      s_xisqr = foldl (+) 0 (fmap (** 2) r1)
      
      a1 = (n * s_xizi - s_xi * s_zi) / (n * s_xisqr - s_xi ** 2)
      a0 = 1 / n * s_zi - a1 * 1 / n * s_xi
      n = fromIntegral $ lengthR r1
      
      a = exp a0
      b = a1

errMsg :: String -> String
errMsg str = "DAT.Math.Statistics.Regression.Exponential: " ++ str