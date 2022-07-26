module Main where

import System.Random.Shuffle ( shuffle' )
import System.Random ( mkStdGen )

import SelectionSort ( ssort )

randomList :: [Int]
randomList = shuffle' [1..20] 20 (mkStdGen 15)

sortedList :: [Int]
sortedList = ssort randomList

main :: IO ()
main = do
    print randomList
    print sortedList