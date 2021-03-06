{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-} 

module DAT.Table 
(
  Table (..),
  appendT,
  prependT,
  concatT,
  joinT,
  mkTable,
  toTable,
  toColumn,
  toRow,
  headTbl,
  tailTbl,
  ithRow,
  ithRow',
  eraseEmptyRows,
  ithCol,
  numRows,
  numCols,
  mapRows,
  mapRows',
  foldRows,
  filterRows,
  filterT,
  sortT,
  eraseRow,
  eraseCol,
  substituteCol,
  numItems
) where

import DAT.Row
import DAT.Math.Matrix
import Control.Monad
import Data.List

import qualified DAT.Math.Vector as V

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
  mul Empty _ = Empty
  mul _ Empty = Empty
  mul t1@(ConsT r1 rs1) t2
    | numCols t1 /= numRows t2 = error $ "Invalid matrices size in multiplication"
    | otherwise = appendT (toTable (calc' r1 t2)) (mul rs1 t2)
    where
      calc' _ Empty = Row []
      calc' _ (ConsT (Row []) _) = Row []
      calc' r1 t = 
        concatR (Row ([V.dotProduct r1 (toRow (mapRows headR t))])) (calc' r1 (mapRows tailR t))

  identity n = helper 1 n Empty
    where
      helper start n m
        | start == n = appendT m (ConsT (mkRw start) Empty)
        | otherwise = helper (start + 1) n (appendT m (ConsT (mkRw start) Empty))

      mkRw k = Row (replaceNth (k - 1) 1 (replicate n 0))

      replaceNth _ _ [] = []
      replaceNth n newVal (x:xs)
        | n == 0 = newVal:xs
        | otherwise = x : replaceNth (n-1) newVal xs

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

toColumn :: Row a -> Table a
toColumn (Row []) = Empty
toColumn (Row (x : xs)) = ConsT (Row [x]) (toColumn (Row xs))

toRow :: Table a -> Row a
toRow t = foldRows (\acc r -> concatR r acc) (Row []) t

headTbl :: Table a -> Table a
headTbl (ConsT r _) = ConsT r Empty
headTbl Empty = Empty
  
tailTbl :: Table a -> Table a
tailTbl (ConsT h t) = t
tailTbl (ConsT h Empty) = Empty
tailTbl Empty = Empty
  
ithRow :: Int -> Table a -> Maybe (Table a)
ithRow i (ConsT r t)
  | i < 0  = Nothing
  | i == 0 = Just $ ConsT r Empty
  | i > 0 = case t of (ConsT nr _) -> ithRow (i - 1) t
                      Empty -> Nothing
ithRow _ Empty = Nothing

ithRow' :: Int -> Table a -> Row a
ithRow' i (ConsT r t)
  | i < 0  = error $ errMsg "index too large"
  | i == 0 = r
  | i > 0 = case t of (ConsT nr _) -> ithRow' (i - 1) t
                      Empty -> error $ errMsg "empty table"
ithRow' _ Empty = error $ errMsg "empty table"

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

mapRows' :: (Row a -> Int -> Row b) -> Table a -> Table b
mapRows' f t = helper f t 0
  where
    helper f (ConsT r rs) acc = ConsT (f r acc) (helper f rs (acc + 1))
    helper f (ConsT r Empty) acc = ConsT (f r acc) Empty
    helper _ Empty _ = Empty
mapRows' _ Empty = Empty

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

sortT :: Ord a => Int -> Table a -> Table a
sortT _ Empty = Empty
sortT id t@(ConsT r rs) = appendT (appendT (sortT id small) mid) (sortT id large)
  where
    small = filterRows (\rw -> (rw `ith` id) < (r `ith` id)) rs
    mid   = appendT (filterRows (\rw -> (rw `ith` id) == (r `ith` id)) rs) (toTable r)
    large = filterRows (\rw -> (rw `ith` id) > (r `ith` id)) rs

numItems :: Table a -> Int
numItems t = foldl (+) 0 fr
  where fr = foldRows (\acc r -> appendItem acc (lengthR r)) (Row []) t

eraseRow :: Int -> Table a -> Table a
eraseRow _ Empty = Empty
eraseRow n t
  | n == 0 = tailTbl t
  | n >= nRows = error $ errMsg "row number to errase should not exceed num of given table rows"
  | otherwise = erraseRow' (n - 1) (headTbl t) (tailTbl t)
    where 
      erraseRow' n last next
        | n == 0 = appendT last (tailTbl next)
        | otherwise = erraseRow' (n - 1)  (appendT last (headTbl next)) (tailTbl next)
      
      nRows = numRows t
    
eraseCol :: Int -> Table a -> Table a 
eraseCol _ Empty = Empty
eraseCol n t
  | n >= nCols = error $ errMsg "row number to errase should not exceed num of given table cols"
  | otherwise = mapRows (\r -> eraseIth n r) t
    where
      nCols = numCols t

substituteCol :: Int -> Table a -> Table a -> Table a
substituteCol _ _ Empty = Empty
substituteCol _ Empty _ = error $ errMsg "could not substitute with empty column"
substituteCol n col t
  | n >= nCols = error $ errMsg "row number to substitute should not exceed num of given table cols"
  | otherwise = mapRows' (\r i -> substituteIth r n (colIth i)) t
    where
      colIth i = (ithRow' i col) `ith` 0
      nRows = numRows t
      nCols = numCols t

errMsg :: String -> String
errMsg str = "DAT.Table: " ++ str