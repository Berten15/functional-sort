module MergeSort
    ( msort
    ) where

msort :: [Int] -> [Int]
msort [] = []
msort [x] = [x]
msort list = do
    let (firstHalf, lastHalf) = splitAt (length list `div` 2) list
    merge (msort firstHalf) (msort lastHalf)

merge :: [Int] -> [Int] -> [Int]
merge x [] = x
merge [] x = x
merge (x:xs) (y:ys) =
    if x < y then x : merge xs (y:ys)
    else y : merge ys (x:xs)