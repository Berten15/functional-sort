module SelectionSort
    ( ssort
    ) where

import Data.List (delete)

ssort :: [Int] -> [Int]
ssort [] = []
ssort list = do
    let min = minimum list
    min : ssort (delete min list)