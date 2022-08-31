module Main where

import System.Random.Shuffle ( shuffle' )
import Text.Printf ( printf )
import Control.DeepSeq ( NFData(..), deepseq )
import System.CPUTime ( getCPUTime )
import Control.Exception ( evaluate )
import Data.List ( sort )

import SelectionSort ( ssort )
import QuickSort ( qsort)
import InsertionSort (isort)
import MergeSort ( msort )
import BubbleSort ( bsort )
import RandomList ( getRandomList, getNearlySortedList, getReverseSortedList, getFewUniqueList)
import Control.Monad (replicateM)
import GHC.Read (list)

-- parameters:
listLength :: Int
listLength = 10^4
nTimes :: Int
nTimes = 10^2
seed :: Int
seed = 1 :: Int


cases :: [(String, Int -> Int ->[Int])]
cases = [("Random", getRandomList),
         ("Nearly Sorted", getNearlySortedList),
         ("Inverse Sorted", getReverseSortedList),
         ("Few Unique", getFewUniqueList)]
algorithms :: [(String, [Int] -> [Int])]
algorithms = [("SelectionSort", ssort),
              ("QuickSort", qsort),
              ("InsertionSort", isort),
              ("MergeSort", msort),
              ("Built-in", sort)]
              -- ("BubbleSort", bsort)] -- bubbleSort is too slow :/

main :: IO ()
main = do
    printf "\n"
    printf "/------------------------------------------------\\\n"
    printf "|                                                |\n"
    printf "|   Benchmark of functional sorting algorithms   |\n"
    printf "|                                                |\n"
    printf "\\------------------------------------------------/\n"
    printf "\nSorting lists of length %d, repeated %d times.\n\n" listLength nTimes

    printf $ replicate 16 ' ' ++ "|"
    printCases cases
    printTable algorithms cases
    printf "\n"


-- Prints cases in header of the table
printCases :: [(String, Int -> Int ->[Int])] -> IO ()
printCases [] = printf "\n"
printCases cases = do
    printf " %14s |" (fst $ head cases)
    printCases (tail cases)


-- Prints entire table
printTable :: [(String, [Int] -> [Int])] -> [(String, Int -> Int ->[Int])] -> IO ()
printTable [] cases = return ()
printTable algorithms cases = do
    printf $ replicate 85 '-' ++ "\n"
    printf "%15s |" (fst $ head algorithms)
    printAlgorithm (snd $ head algorithms) cases
    printTable (tail algorithms) cases


-- Prints one line of the table corresponding to one algorithm
printAlgorithm :: ([Int] -> [Int]) -> [(String, Int -> Int ->[Int])] -> IO ()
printAlgorithm algorithm [] = printf "\n"
printAlgorithm algorithm cases = do
    duration <- timeN nTimes algorithm (snd $ head cases) listLength seed
    printTime $ duration / fromIntegral nTimes
    printAlgorithm algorithm (tail cases)


-- Measures execution time of algorithm n times
timeN :: Int -> ([Int] -> [Int]) -> (Int -> Int ->[Int]) -> Int -> Int -> IO Double
timeN 0 algorithm getList listLength seed = return 0.0
timeN n algorithm getList listLength seed = do
    duration <- timeOnce algorithm getList listLength seed
    rest <- timeN (n-1) algorithm getList listLength (seed+1)
    return (duration + rest)

-- Measuers execution time of algorithm once
timeOnce :: ([Int] -> [Int]) -> (Int -> Int ->[Int]) -> Int -> Int -> IO Double
timeOnce algorithm getList listLength seed = do
    let toSort = getList listLength seed
    start <- toSort `deepseq` getCPUTime
    let sorted = algorithm toSort
    end   <- sorted `deepseq` getCPUTime
    return (fromIntegral (end - start) / (10^9))


printTime :: Double -> IO ()
printTime time
    | time < 1      = printf "\x1b[32m %11.2f μs \x1b[0m|" (time * 1000.0)
    | time > 1000   = printf "\x1b[31m %11.2f  s \x1b[0m|" (time / 1000.0)
    | otherwise     = printf "\x1b[33m %11.2f ms \x1b[0m|" time