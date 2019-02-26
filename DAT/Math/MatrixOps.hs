module DAT.Math.MatrixOps (
  minor,
  determinant,
  cofactorMatrix,
  transpose',
  inverse,
  remove
) where 

import DAT.Table
import DAT.Row
import DAT.Math.Matrix

transpose' :: Table a -> Table a
transpose' Empty = Empty
transpose' (ConsT (Row []) _) = Empty
transpose' t@(ConsT r rs) = 
  appendT (toTable $ toRow $ mapRows headR t) (transpose' (mapRows tailR t))

determinant :: Table Double -> Double
determinant (ConsT (Row [a, c]) (ConsT (Row [b, d]) Empty)) = a * d - b * c
determinant t 
  | nRows /= nCols = error $ errMsg "matrix should be squared"
  | otherwise = determinant' 0 t
    where
      determinant' j mx
        | j == nCols = 0
        | otherwise = 
          (- 1) ** (fromIntegral j) * (a1 `ith` j) * (minor 0 j mx) + (determinant' (j + 1) mx)

      nRows = numRows t
      nCols = numCols t
      a1 = toRow $ headTbl t

minor :: Int -> Int -> Table Double -> Double
minor i j t = determinant erased
  where
    erased = remove t i j      

cofactor :: Table Double -> Int -> Int -> Double
cofactor t i j = (- 1) ** (fromIntegral i + fromIntegral j) * (minor i j t)

cofactorMatrix :: Table Double -> Table Double
cofactorMatrix t = mkTable [ [(cofactor t i j) | j <- [0..n] ] | i <- [0..n] ]
  where
    n = numRows t - 1

inverse :: Table Double -> Table Double
inverse t = transpose' $ fmap (\x -> x / det) cof
  where 
    cof = cofactorMatrix t
    det = determinant t

remove :: Table a -> Int -> Int -> Table a
remove t i j
  | i >= nRows = error $ errMsg "i should not exceed number of rows"
  | j >= nCols = error $ errMsg "j should not exceed number of cols"
  | nRows /= nCols = error $ errMsg "matrix should be squared"
  | otherwise = eraseRow i (eraseCol j t)
    where
      nRows = numRows t
      nCols = numCols t  

errMsg :: String -> String
errMsg str = "DAT.Math.MatrixOps: " ++ str

-- let t = ConsT (Row [3, 0, 2]) (ConsT (Row [2, 0, -2]) (ConsT (Row [0, 1, 1]) Empty))