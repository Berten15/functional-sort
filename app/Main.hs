module Main where

import System.Random.Shuffle ( shuffle' )
import System.Random ( mkStdGen )

import SelectionSort ( ssort )
import QuickSort ( qsort )
import InsertionSort (isort)

randomList :: [Int]
randomList = shuffle' [1..20] 20 (mkStdGen 15)

sortedList :: [Int]
sortedList = isort randomList

main :: IO ()
main = do
    print randomList
    print sortedList