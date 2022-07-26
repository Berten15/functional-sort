module QuickSort
    ( qsort
    ) where

qsort :: [Int] -> [Int]
qsort [] = []
qsort list = qsort [x | x <- tail list, x <= head list] ++ [head list] ++ qsort [x | x <- tail list, x > head list]