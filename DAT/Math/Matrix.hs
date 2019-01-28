{-# LANGUAGE MultiParamTypeClasses #-} 

module DAT.Math.Matrix where 

class Monad m => Matrix m a where
  mul :: Num a =>  m a -> m a -> m a
  transpose' :: m a -> m a
  dotProduct ::  Num a => m a -> m a -> a
  toScalar :: Num a => a -> m a -> m a