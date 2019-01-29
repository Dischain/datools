{-# LANGUAGE MultiParamTypeClasses #-} 

module DAT.Math.Vector where 

class Monad m => Vector m a where
  dotProduct :: Num a => m a -> m a -> a
  size :: Foldable m => m a -> Int
  genericSize :: Num s => m a -> s

  sum' :: (Applicative m, Num a) => m a -> m a -> m a
  sum' v1 v2 = (+) <$> v1 <*> v2

  div' :: (Applicative m, Num a) => m a -> m a -> m a
  div' v1 v2 = (-) <$> v1 <*> v2