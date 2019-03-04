{-# LANGUAGE MultiParamTypeClasses #-} 

module DAT.Math.Matrix where 

class Monad m => Matrix m a where
  mul :: Num a =>  m a -> m a -> m a
  
  scalarMul :: Num a => a -> m a -> m a
  scalarMul a m = fmap (* a) m

  sum' :: (Num a, Applicative m) => m a -> m a -> m a
  sum' m1 m2 = (+) <$> m1 <*> m2

  pow' :: (Num a, Applicative m) => m a -> Int -> m a
  pow' m n = (iterate (\m -> m `mul` m) m) !! (n - 1)

  identity :: Int -> m a