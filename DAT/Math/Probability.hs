module DAT.Math.Probability
(
  mean,
  covariance,
  expectation,
  covariance,
  sampleCovariance
) where

import DAT.Row 
import DAT.Math.Probability.DiscreteRandomVar 

mean :: Row Double -> Double
mean (Row []) = error $ errMsg "can not calculate mean of an empty sample"
mean r = foldl (+) 0 r / (fromIntegral $ lengthR r)

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

-- | Sample covariance between two selections
sampleCovariance :: Row Double -> Row Double -> Double
sampleCovariance r1 r2 = mean ((*) <$> 
                              (fmap (\x1 -> x1 - mean1) r1) <*> 
                              (fmap (\x2 -> x2 - mean2) r2))
  where
    mean1 = mean r1
    mean2 = mean r2

errMsg :: String -> String
errMsg str = "DAT.Math.Probability: " ++ str
