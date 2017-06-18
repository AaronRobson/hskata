module Kata06 where

import Data.List (sort)
import Data.Char (isAscii, isAlpha, toLower)
import Data.List (intercalate)

--http://codekata.com/kata/kata06-anagrams/

signature :: String -> String
signature = sort . map toLower . filter isAlpha . filter isAscii

areAnagrams :: String -> String -> Bool
areAnagrams x y = areAnagramsList [x,y]

adjacentPairs :: [a] -> [(a,a)]
adjacentPairs = zip <*> tail

allAreEqual ::  (Eq a) => [a] -> Bool
allAreEqual = (all (uncurry (==))) . adjacentPairs

areAnagramsList :: [String] -> Bool
areAnagramsList = allAreEqual . map signature

findAnagrams :: [String] -> [[String]]
findAnagrams = undefined

formatAnagram :: [String] -> String
formatAnagram = intercalate " "

formatAnagrams :: [[String]] -> [String]
formatAnagrams = map formatAnagram

main :: IO ()
main = do
  putStrLn "CodeKata06:\n"
  let filePath = "wordlist.txt"
  putStrLn $ "Loading words from " ++ filePath
  allWords <- fmap lines $ readFile filePath
  let anagrams = formatAnagrams $ findAnagrams allWords
  putStrLn $ "Found the following:"
  mapM_ putStrLn $ anagrams
  putStrLn "\n----------------\nEnd.\n"
