module RandomList
    (
        getRandomList,
        getNearlySortedList,
        getReverseSortedList,
        getFewUniqueList
    ) where

import System.Random.Shuffle ( shuffle' )
import System.Random ( mkStdGen )

getRandomList :: Int -> Int -> [Int]
getRandomList listLength seed = shuffle' [1..listLength] listLength (mkStdGen seed)


getNearlySortedList :: Int -> Int -> [Int]
getNearlySortedList listLength = randomiseCouple [1..listLength]

-- Replaces one element every 15 elements in list with 4, first replacement depends on seed
randomiseCouple :: [Int] -> Int -> [Int]
randomiseCouple [] seed = []
randomiseCouple list 0 = 4 : randomiseCouple (tail list) 15
randomiseCouple list seed = head list : randomiseCouple (tail list) (seed-1)


getReverseSortedList :: Int -> Int -> [Int]
getReverseSortedList listLength seed = [listLength..1]

-- Returns list with all elements in [1..10]
-- listLength should be multiple of 10
getFewUniqueList :: Int -> Int -> [Int]
getFewUniqueList listLength seed = do
    let list = concatMap (replicate $ listLength `div` 10) [1..10]
    shuffle' list listLength (mkStdGen seed)