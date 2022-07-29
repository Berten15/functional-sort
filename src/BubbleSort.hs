module BubbleSort
    ( bsort
    ) where

bsort :: [Int] -> [Int]
bsort [] = []
bsort list = do
    let newList = traverse' list
    bsort (init newList) ++ [last newList]

traverse' :: [Int] -> [Int]
traverse' [] = []
traverse' [x] = [x]
traverse' (x:y:s) =
    if x < y then x : traverse' (y:s)
    else y : traverse' (x:s)