module DAT.Math.MatrixOps (
  minor,
  determinant
) where 

import DAT.Table
import DAT.Row
import DAT.Math.Matrix

determinant :: Floating a => Table a -> a
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

minor :: Floating a =>  Int -> Int -> Table a -> a
minor i j t
  | i >= nRows = error $ errMsg "i should not exceed number of rows"
  | j >= nCols = error $ errMsg "j should not exceed number of cols"
  | nRows /= nCols = error $ errMsg "matrix should be squared"
  | otherwise = determinant erased

  where
    erased = eraseRow i (eraseCol j t)
    
    nRows = numRows t
    nCols = numCols t    

errMsg :: String -> String
errMsg str = "DAT.Math.MatrixOps: " ++ str
