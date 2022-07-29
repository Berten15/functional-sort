import System.Random.Shuffle ( shuffle' )
import System.Random ( mkStdGen )
import Data.List ( sort )

import SelectionSort ( ssort )
import QuickSort ( qsort)
import InsertionSort (isort)

randomList :: [Int]
randomList = shuffle' [1..1000] 1000 (mkStdGen 15)

algorithms = [("SelectionSort", ssort), ("QuickSort", qsort), ("InsertionSort", isort), ("MergeSort", isort)]
cases = [("Random List", randomList), ("Singleton List", [15]), ("Empty List", [])]

main :: IO ()
main = checkAll cases algorithms

-- Checks all algorithms case by case
checkAll :: [(String, [Int])] -> [(String, [Int] -> [Int])] -> IO ()
checkAll [] algorithms = return ()
checkAll cases algorithms = do
    putStrLn ("\nSorting" ++ fst (head cases) ++ ":\n")
    checkCase algorithms (snd $ head cases)
    checkAll (tail cases) algorithms

-- Checks all algorithms for the given case
checkCase :: [(String, [Int] -> [Int])] -> [Int] -> IO ()
checkCase [] toSort = return ()
checkCase algorithms toSort = do
    putStrLn (uncurry check (head algorithms) randomList)
    checkCase (tail algorithms) toSort

-- returns String indicating if sort was succesful
check :: String -> ([Int] -> [Int]) -> [Int] -> String
check name algorithm list = do
    let result = if algorithm list == sort list then "SUCCESS!" else "FAIL!"
    "  - " ++ name ++ ": " ++ result