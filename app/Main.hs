module Main where

import System.Random.Shuffle ( shuffle' )
import System.Random ( mkStdGen )
import Text.Printf ( printf )
import Control.DeepSeq ( NFData(..) )
import System.CPUTime ( getCPUTime )
import Control.Exception ( evaluate )

import SelectionSort ( ssort )
import QuickSort ( qsort)
import InsertionSort (isort)
import MergeSort ( msort )
import BubbleSort ( bsort )

-- parameters:
listLength :: Int
listLength = 10^3

randomList :: [Int]
randomList = shuffle' [1..listLength] listLength (mkStdGen 15)

sortedList :: [Int]
sortedList = isort randomList


cases :: [(String, [Int])]
cases = [("Random", randomList), ("Nearly Sorted", [1, 2, 4, 3]), ("Inverse Sorted", [4, 3, 2, 1]), ("Few Unique", [3, 1, 3, 1])]
algorithms :: [(String, [Int] -> [Int])]
algorithms = [("SelectionSort", ssort), ("QuickSort", qsort), ("InsertionSort", isort), ("MergeSort", msort), ("BubbleSort", bsort)]


main :: IO ()
main = do
    printf "\n\n\n"
    printf $ replicate 16 ' ' ++ "|"
    printCases cases
    printTable algorithms cases


printCases :: [(String, [Int])] -> IO ()
printCases [] = printf "\n"
printCases cases = do
    printf " %14s |" (fst $ head cases)
    printCases (tail cases)


printTable :: [(String, [Int] -> [Int])] -> [(String, [Int])] -> IO ()
printTable [] cases = return ()
printTable algorithms cases = do
    printf $ replicate 80 '-' ++ "\n"
    printf "%15s |" (fst $ head algorithms)
    printAlgorithm (snd $ head algorithms) cases
    printTable (tail algorithms) cases


printAlgorithm :: ([Int] -> [Int]) -> [(String, [Int])] -> IO ()
printAlgorithm algorithm [] = printf "\n"
printAlgorithm algorithm cases = do
    duration <- time $ algorithm $ snd $ head cases
    printf " %-11.2f ms |" duration
    printAlgorithm algorithm (tail cases)


-- function to measure execution time in ms (adapted from the Haskell Wiki)
time :: (Num t, NFData t) => [t] -> IO Double
time y = do
    start <- getCPUTime
    x <- evaluate y
    end   <- getCPUTime
    return (fromIntegral (end - start) / (10^9))