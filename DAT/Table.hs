module DAT.Table where

import DAT.Row
import Text.CSV

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
  (>>=) (ConsT col Empty) f = case (fmap f col) of 
    Row tbls@(t:ts) -> joinT tbls
    Row [] -> Empty
  (>>=) (ConsT col cols) f  = appendT ((ConsT col Empty) >>= f) (cols >>= f)
  
  return a = ConsT (Row [a]) Empty
  
appendT :: Table a -> Table a -> Table a
appendT Empty Empty = Empty
appendT t1 Empty = t1
appendT Empty t2 = t2
appendT (ConsT a as) bs = ConsT a (appendT as bs)
  
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
modifyRows :: (Row a -> Row b) -> Table a -> Table b
modifyRows f (ConsT r rs) = ConsT (f r) (modifyRows f rs)
modifyRows _ Empty = Empty
  
filterRows :: (Row a -> Bool) -> Table a -> Table a
filterRows f (ConsT r rs)
  | f r = ConsT r (filterRows f rs)
  | otherwise = filterRows f rs
filterRows _ Empty = Empty
  
ithCol :: Int -> Table a -> Maybe (Table a)
ithCol i t@(ConsT r rs)
  | i < 0 = Nothing
  | otherwise = Just $ modifyRows (\r -> Row [(r `ith` i)]) t
ithCol i Empty = Nothing
  
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
      | r `ith` c1 == b `ith` c2 = r `concatR` (eraseIth c2 b)
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
      | r `ith` c1 == b `ith` c2 = r `concatR` (eraseIth c2 b)
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
      | r `ith` c1 == b `ith` c2 = r `concatR` (eraseIth c2 b)
      | otherwise = joinOne c1 r c2 bs
  
eraseEmptyRows :: Table a -> Table a
eraseEmptyRows Empty = Empty
eraseEmptyRows (ConsT (Row []) rs) = rs
eraseEmptyRows (ConsT r@(Row (a : as)) rs) = ConsT r (eraseEmptyRows rs)

mkTableFromCSV :: FilePath -> IO (Table Field)
mkTableFromCSV p = 
  (parseCSVFromFile p) >>= (\csv -> return $ either (\err -> Empty) (\c -> mkTable c) csv) 
