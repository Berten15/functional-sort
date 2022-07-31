module Main where

import System.Random.Shuffle ( shuffle' )
import Text.Printf ( printf )
import Control.DeepSeq ( NFData(..) )
import System.CPUTime ( getCPUTime )
import Control.Exception ( evaluate )

import SelectionSort ( ssort )
import QuickSort ( qsort)
import InsertionSort (isort)
import MergeSort ( msort )
import BubbleSort ( bsort )
import RandomList ( getRandomList, getNearlySortedList, getReverseSortedList, getFewUniqueList)

-- parameters:
listLength :: Int
listLength = 10^3
nTimes :: Int
nTimes = 10^3

seed = 1 :: Int


cases :: [(String, Int -> Int ->[Int])]
cases = [("Random", getRandomList), ("Nearly Sorted", getNearlySortedList), ("Inverse Sorted", getReverseSortedList), ("Few Unique", getFewUniqueList)]
algorithms :: [(String, [Int] -> [Int])]
algorithms = [("SelectionSort", ssort), ("QuickSort", qsort), ("InsertionSort", isort), ("MergeSort", msort), ("BubbleSort", bsort)]


main :: IO ()
main = do
    printf "\n\n\n"
    printf $ replicate 16 ' ' ++ "|"
    printCases cases
    printTable algorithms cases



printCases :: [(String, Int -> Int ->[Int])] -> IO ()
printCases [] = printf "\n"
printCases cases = do
    printf " %14s |" (fst $ head cases)
    printCases (tail cases)


printTable :: [(String, [Int] -> [Int])] -> [(String, Int -> Int ->[Int])] -> IO ()
printTable [] cases = return ()
printTable algorithms cases = do
    printf $ replicate 80 '-' ++ "\n"
    printf "%15s |" (fst $ head algorithms)
    printAlgorithm (snd $ head algorithms) cases
    printTable (tail algorithms) cases


printAlgorithm :: ([Int] -> [Int]) -> [(String, Int -> Int ->[Int])] -> IO ()
printAlgorithm algorithm [] = printf "\n"
printAlgorithm algorithm cases = do
    let toSort = (snd $ head cases) listLength seed
    duration <- time (algorithm toSort)
    printf " %-11.2f ms |" (duration )-- / fromIntegral nTimes)
    printAlgorithm algorithm (tail cases)


-- function to measure execution time in ms (adapted from the Haskell Wiki)
time :: (Num t, NFData t) => [t] -> IO Double
time y = do
    start <- getCPUTime
    x <- evaluate y
    end   <- getCPUTime
    return (fromIntegral (end - start) / (10^9))