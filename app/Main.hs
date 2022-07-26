module Main where

import Lib ()
import System.Random.Shuffle ( shuffle' )
import System.Random ( mkStdGen, StdGen )
import Data.List ( sort )


list :: [Int]
list = [1..20]

gen :: StdGen
gen = mkStdGen 15

shuffledList :: [Int]
shuffledList = shuffle' list (length list) gen

sortedList :: [Int]
sortedList = sort shuffledList

main :: IO ()
main = do
    print list
    print shuffledList
    print sortedList