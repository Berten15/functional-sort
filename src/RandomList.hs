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

randomiseCouple :: [Int] -> Int -> [Int]
randomiseCouple [] seed = []
randomiseCouple list 0 = 4 : randomiseCouple (tail list) 15
randomiseCouple list seed = head list : randomiseCouple (tail list) (seed-1)


getReverseSortedList :: Int -> Int -> [Int]
getReverseSortedList listLength seed = [listLength..1]


-- listLength should be multiple of 50 
getFewUniqueList :: Int -> Int -> [Int]
getFewUniqueList listLength seed = do
    let list = [50*x | x <- [1..(listLength `div` 50)]] >>= replicate 50 
    shuffle' list listLength (mkStdGen seed)