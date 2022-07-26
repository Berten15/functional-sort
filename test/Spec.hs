import System.Random.Shuffle ( shuffle' )
import System.Random ( mkStdGen )
import Data.List ( sort )

import SelectionSort ( ssort )

randomList :: [Int]
randomList = shuffle' [1..1000] 1000 (mkStdGen 15)

emptyList :: [Int]
emptyList = []

singletonList :: [Int]
singletonList = [15]

main :: IO ()
main = do
    putStrLn "\nSorting Random List:\n"
    putStrLn (check "SelectionSort" ssort randomList)

    putStrLn "\nSorting Empty List:\n"
    putStrLn (check "SelectionSort" ssort emptyList)

    putStrLn "\nSorting Singleton List:\n"
    putStrLn (check "SelectionSort" ssort singletonList)


-- returns String indicating if sort was succesful
check :: String -> ([Int] -> [Int]) -> [Int] -> String
check name algorithm list = do
    let result = if algorithm list == sort list then "SUCCESS!" else "FAIL!"
    "  - " ++ name ++ ": " ++ result