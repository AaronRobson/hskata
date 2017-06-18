module TestKata06 where

import Test.HUnit
import Kata06 (signature, areAnagrams, areAnagramsList)

--http://codekata.com/kata/kata06-anagrams/

signatureTest :: Test
signatureTest = TestList
  [ TestCase $ "" @=? signature ""
  , TestCase $ "a" @=? signature "a"
  , TestCase $ "a" @=? signature "A"
  , TestCase $ "ab" @=? signature "ab"
  , TestCase $ "ab" @=? signature "ba"
  , TestCase $ "abc" @=? signature " A \n B , ^&*<> C"
  ]

areAnagramsTest :: Test
areAnagramsTest = TestList
  [ TestCase $ True @=? areAnagrams "" ""
  , TestCase $ False @=? areAnagrams "" "a"
  , TestCase $ True @=? areAnagrams "a" "a"
  , TestCase $ True @=? areAnagrams "sas" "ass"
  , TestCase $ True @=? areAnagrams "SAS" "Ass"
  , TestCase $ True @=? areAnagrams "kinship" "pinkish"
  , TestCase $ True @=? areAnagrams "Florence Nightingale" "Flit on, cheering angel"
  ]

areAnagramsListTest :: Test
areAnagramsListTest = TestList
  [ TestCase $ True @=? areAnagramsList []
  , TestCase $ True @=? areAnagramsList ["test"]
  , TestCase $ False @=? areAnagramsList ["test", "rest"]
  , TestCase $ True @=? areAnagramsList ["rots", "sort"]
  , TestCase $ True @=? areAnagramsList ["crepitus", "cuprites", "pictures", "piecrust"]
  ]

main :: IO Counts
main = runTestTT $ TestList [signatureTest, areAnagramsTest, areAnagramsListTest]
