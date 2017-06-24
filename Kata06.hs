module Kata06 where

import Data.List (sort)
import Data.Char (isAscii, isPrint, isAlpha, isPunctuation, isSpace, toLower)
import Data.List (intercalate, groupBy, nub)
import Data.Function (on)
import Data.Maybe (catMaybes)
import Data.String.Utils (strip) -- cabal install missingh

--http://codekata.com/kata/kata06-anagrams/

whiteSpaceToSpace :: Char -> Char
whiteSpaceToSpace x = if isSpace x then ' ' else x

validate :: String -> String
validate = map toLower . strip . filter isPrint . filter isAscii . map whiteSpaceToSpace

validateCheck :: String -> Maybe String
validateCheck xs = if any isPunctuation xs
                     then Nothing
                     else Just $ validate xs

validateMany :: [String] -> [String]
validateMany = catMaybes . map validateCheck

signature :: String -> String
signature = sort . filter isAlpha . validate

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
signatureWords wordList = zip (signatures validated) validated
    where validated = validateMany wordList

groupedSignatureWords :: [String] -> [[(String,String)]]
groupedSignatureWords = (groupBy ((==) `on` fst)) . sort . signatureWords

groupedWords :: [String] -> [[String]]
groupedWords = (map (map snd)) . groupedSignatureWords

removeNonMultiples :: [[a]] -> [[a]]
removeNonMultiples = filter ((>1) . length)

groupedWordsWithAnagrams :: [String] -> [[String]]
groupedWordsWithAnagrams = removeNonMultiples . map nub . groupedWords

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
