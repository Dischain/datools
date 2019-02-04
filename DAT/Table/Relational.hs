module DAT.Table.Relational where

import DAT.Table
import DAT.Row
import Data.List
import Control.Monad

select :: Int -> (a -> Bool) -> Table a -> Table a
select colNum f t = filterRows (\r -> f (r `ith` colNum)) t
  
selectOr :: Int -> (a -> Bool) -> Int -> (a -> Bool) -> Table a -> Table a
selectOr c1 f1 c2 f2 t = filterRows (\r -> f1 (r `ith` c1) || f2 (r `ith` c2)) t
  
innerJoin :: Eq a => Int -> Table a -> Int -> Table a -> Table a
innerJoin _ Empty _ _ = Empty
innerJoin _ _ _ Empty = Empty
innerJoin c1 t1@(ConsT a as) c2 t2 =
  eraseEmptyRows $ ConsT (joinOne c1 a c2 t2) (innerJoin c1 as c2 t2)
  where
    joinOne :: Eq a => Int -> Row a -> Int -> Table a -> Row a
    joinOne c1 r c2 Empty = Row []
    joinOne c1 r c2 (ConsT b bs) 
      | r `ith` c1 == b `ith` c2 = r `concatR` (splice c2 b)
      | otherwise = joinOne c1 r c2 bs
  
leftJoin :: Eq a => Int -> Table a -> Int -> Table a -> Table a
leftJoin _ Empty _ _ = Empty
leftJoin _ _ _ Empty = Empty
leftJoin c1 t1@(ConsT a as) c2 t2 =
  ConsT (joinOne c1 a c2 t2) (leftJoin c1 as c2 t2)
  where
    joinOne :: Eq a => Int -> Row a -> Int -> Table a -> Row a
    joinOne c1 r c2 Empty = r 
    joinOne c1 r c2 (ConsT b bs) 
      | r `ith` c1 == b `ith` c2 = r `concatR` (splice c2 b)
      | otherwise = joinOne c1 r c2 bs

rightJoin :: Eq a => Int -> Table a -> Int -> Table a -> Table a
rightJoin _ Empty _ _ = Empty
rightJoin _ _ _ Empty = Empty
rightJoin c1 t1 c2 t2@(ConsT a as) =
  ConsT (joinOne c2 a c1 t1) (rightJoin c1 t1 c2 as)
  where
    joinOne :: Eq a => Int -> Row a -> Int -> Table a -> Row a
    joinOne c1 r c2 Empty = r 
    joinOne c1 r c2 (ConsT b bs) 
      | r `ith` c1 == b `ith` c2 = r `concatR` (splice c2 b)
      | otherwise = joinOne c1 r c2 bs