module Kata06 where

import Data.List (sort)
import Data.Char (isAscii, isAlpha, toLower)
import Data.List (intercalate, groupBy)
import Data.Function (on)

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

signatures :: [String] -> [String]
signatures = map signature

signatureWords :: [String] -> [(String,String)]
signatureWords wordList = zip (signatures wordList) wordList

groupedSignatureWords :: [String] -> [[(String,String)]]
groupedSignatureWords = (groupBy ((==) `on` fst)) . sort . signatureWords

groupedWords :: [String] -> [[String]]
groupedWords = (map (map snd)) . groupedSignatureWords

removeNonMultiples :: [[a]] -> [[a]]
removeNonMultiples = filter ((>1) . length)

groupedWordsWithAnagrams :: [String] -> [[String]]
groupedWordsWithAnagrams = removeNonMultiples . groupedWords

findAnagrams :: [String] -> [[String]]
findAnagrams = groupedWordsWithAnagrams

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
