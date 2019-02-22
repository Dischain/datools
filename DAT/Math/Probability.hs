module DAT.Math.Probability
(
  covariance,
  expectation,
) where

import DAT.Row 
import DAT.Table
import DAT.Math.Matrix
import DAT.Math.Probability.DiscreteRandomVar 
import Math.Combinatorics.Exact.Binomial

expectation :: DRV -> Double
expectation drv@(DRV vals probs) = 
  foldl (+) 0 ((*) <$> vals <*> probs)

covariance :: DRV -> DRV -> Double
covariance drv1@(DRV v1 p1) drv2@(DRV v2 p2) =
  foldl (+) 0 
    ((*) <$> 
    (fmap (\x1 -> x1 - expectation1) v1) <*> 
    (fmap (\x2 -> x2 - expectation2) v2))
    
    where
      expectation1 = expectation drv1
      expectation2 = expectation drv2

pmf :: Int -> Int -> Double -> Double
pmf k n p = 
  (fromIntegral (n `choose` k)) * (p ^ k) * ((1 - p) ^ (n - k))

errMsg :: String -> String
errMsg str = "DAT.Math.Probability: " ++ str
