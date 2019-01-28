{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-} 

module DAT.Table where

import DAT.Row
import DAT.Math.Matrix
import Control.Monad
import Data.List

data Table a = Empty | ConsT (Row a) (Table a) deriving Show

instance Functor Table where
  fmap f Empty = Empty
  fmap f (ConsT c cs) = ConsT (fmap f c) (fmap f cs)
  
instance Applicative Table where
  pure a = ConsT (pure a) Empty
  
  Empty <*> _ = Empty
  (ConsT fs fss) <*> t@(ConsT c Empty) = ConsT (fs <*> c) Empty
  (ConsT fs fss) <*> (ConsT bs bss) =
    (ConsT (fs <*> bs) Empty) `appendT` (fss <*> bss)
  
instance Monad Table where
  (>>=) Empty f = Empty
  (>>=) (ConsT r Empty) f = case (fmap f r) of 
    Row tbls@(t:ts) -> joinT tbls
    Row [] -> Empty
  (>>=) (ConsT r rs) f  = appendT ((ConsT r Empty) >>= f) (rs >>= f)
  
  return a = ConsT (Row [a]) Empty

instance Num a => Matrix Table a where
  transpose' Empty = Empty
  transpose' (ConsT (Row []) _) = Empty
  transpose' t@(ConsT r rs) = 
    appendT (toTable $ toRow $ mapRows headR t) (transpose' (mapRows tailR t))

  dotProduct (ConsT (Row r1) Empty) (ConsT (Row r2) Empty) = sum $ zipWith (*) r1 r2
  dotProduct (ConsT (Row []) _) t = 0
  dotProduct t (ConsT (Row []) _) = 0
  dotProduct t Empty = 0
  dotProduct Empty t = 0

  mul Empty _ = Empty
  mul _ Empty = Empty
  mul t1@(ConsT r1 rs1) t2
    | numCols t1 /= numRows t2 = error $ "Invalid matrices to multiply"
    | otherwise = appendT (toTable (calc' r1 t2)) (mul rs1 t2)
    where
      calc' _ Empty = Row []
      calc' _ (ConsT (Row []) _) = Row []
      calc' r1 t = 
        concatR (Row ([dotProduct (toTable r1) (toTable (toRow (mapRows headR t)))])) (calc' r1 (mapRows tailR t))

  toScalar s t = fmap (* s) t
      
appendT :: Table a -> Table a -> Table a
appendT Empty Empty = Empty
appendT t1 Empty = t1
appendT Empty t2 = t2
appendT (ConsT a as) bs = ConsT a (appendT as bs)
  
prependT :: Table a -> Table a -> Table a
prependT which to = appendT to which

-- Concatenating table horizontally
concatT :: Table a -> Table a -> Table a
concatT Empty Empty = Empty
concatT t1 Empty = t1
concatT Empty t2 = t2
concatT (ConsT a as) (ConsT b bs) = ConsT (concatR a b) (concatT as bs)
  
joinT :: [Table a] -> Table a
joinT tbls@((ConsT c cs) : ds) =
    foldl (\acc t-> appendT t acc) Empty (reverse tbls)
  
mkTable :: [[a]] -> Table a
mkTable t@(r@(c : cs) : rs) = 
  (foldl (\acc r -> ConsT r acc) Empty) (reverse $ map (\r -> Row r) t)
mkTable [[]] = Empty

toTable :: Row a -> Table a
toTable r@(Row (a : _)) = ConsT r Empty
toTable (Row []) = Empty

toRow :: Table a -> Row a
toRow t = foldRows (\acc r -> concatR r acc) (Row []) t

headTbl :: Table a -> Table a
headTbl (ConsT r _) = ConsT r Empty
headTbl Empty = Empty
  
tailTbl :: Table a -> Table a
tailTbl (ConsT h t@(ConsT _ _)) = t
tailTbl Empty = Empty
  
ithRow :: Int -> Table a -> Maybe (Table a)
ithRow i (ConsT r t)
  | i < 0  = Nothing
  | i == 0 = Just $ ConsT r Empty
  | i > 0 = case t of (ConsT nr _) -> ithRow (i - 1) t
                      Empty -> Nothing
ithRow _ Empty = Nothing

eraseEmptyRows :: Table a -> Table a
eraseEmptyRows Empty = Empty
eraseEmptyRows (ConsT (Row []) rs) = rs
eraseEmptyRows (ConsT r@(Row (a : as)) rs) = ConsT r (eraseEmptyRows rs)

ithCol :: Int -> Table a -> Maybe (Table a)
ithCol i t@(ConsT r rs)
  | i < 0 = Nothing
  | otherwise = Just $ mapRows (\r -> Row [(r `ith` i)]) t
ithCol i Empty = Nothing

numRows :: Table a -> Int
numRows Empty = 0
numRows t@(ConsT r rs) = numRows' t 0
  where
    numRows' (ConsT r rs) acc = numRows' rs (acc + 1)
    numRows' Empty acc = acc
  
numCols :: Table a -> Int
numCols Empty = 0
numCols t = case headTbl t of ConsT (Row r) _ -> length r
                              otherwise -> 0
  
-- Traverse through each row
mapRows :: (Row a -> Row b) -> Table a -> Table b
mapRows f (ConsT r rs) = ConsT (f r) (mapRows f rs)
mapRows _ Empty = Empty

foldRows :: (Row b -> Row a -> Row b) -> Row b -> Table a -> Row b
foldRows f acc (ConsT r rs) = concatR (f acc r) (foldRows f acc rs)
foldRows _ _ Empty = Row []

filterRows :: (Row a -> Bool) -> Table a -> Table a
filterRows f (ConsT r rs)
  | f r = ConsT r (filterRows f rs)
  | otherwise = filterRows f rs
filterRows _ Empty = Empty
  
filterT :: (a -> Bool) -> Table a -> Table a
filterT f (ConsT (Row r) rs)
  | (length $ filter f r) /= 0 = eraseEmptyRows $ ConsT (Row r) (filterT f rs)
  | otherwise = filterT f rs
filterT f Empty = Empty

sortT :: Ord a => Table a -> Int -> Table a
sortT Empty _ = Empty
sortT t@(ConsT r rs) id = appendT (appendT (sortT small id) mid) (sortT large id)
  where
    small = filterRows (\rw -> (rw `ith` id) < (r `ith` id)) rs
    mid   = appendT (filterRows (\rw -> (rw `ith` id) == (r `ith` id)) rs) (toTable r)
    large = filterRows (\rw -> (rw `ith` id) > (r `ith` id)) rs
