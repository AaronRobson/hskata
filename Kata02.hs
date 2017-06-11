module Kata02 where

import Safe (atMay) --cabal install safe
import Data.Maybe (fromMaybe)

--http://codekata.com/kata/kata02-karate-chop/

splitUp :: Int -> [a] -> Maybe (a, [a], [a])
splitUp i xs = do
    nth <- atMay xs i
    return (nth, take i xs, drop (succ i) xs)

middlePoint :: Int -> Int
middlePoint = (`div` 2)

chopMaybe :: (Ord a) => a -> [a] -> Maybe Int
chopMaybe item xs = do
    (nth, before, after) <- splitUp middleIndex xs
    if item == nth
      then return middleIndex
      else
        if item < nth
          then (chopMaybe item before)
          else fmap (+(succ middleIndex)) (chopMaybe item after)
    where
      middleIndex = middlePoint $ length xs

chop :: (Ord a) => a -> [a] -> Int
chop item xs = fromMaybe (-1) $ chopMaybe item xs

main :: IO ()
main = do
    putStrLn "CodeKata02:"
    putStrLn $ "chopped " ++ (show xs) ++ " looking for " ++ (show item)
    putStrLn $ "position " ++ (show answer) ++ " returned."
    where
      item :: Int
      item = 5
      xs :: [Int]
      xs = [1..10]
      answer :: Int
      answer = chop item xs
