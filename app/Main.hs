module Main where

import System.Random.Shuffle ( shuffle' )
import System.Random ( mkStdGen )

import SelectionSort ( ssort )
import QuickSort ( qsort )

randomList :: [Int]
randomList = shuffle' [1..20] 20 (mkStdGen 15)

sortedList :: [Int]
sortedList = qsort randomList

main :: IO ()
main = do
    print randomList
    print sortedList