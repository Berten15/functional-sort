module InsertionSort
    ( isort
    ) where

isort :: [Int] -> [Int]
isort = foldr insert []

-- The last element of the list might need to move up
insert :: Int -> [Int] -> [Int]
insert x [] = [x]
insert x (y:ys) =
    if x < y then x : y : ys
    else y : insert x ys