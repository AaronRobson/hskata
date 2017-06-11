module TestKata02 where

import Test.HUnit
import Kata02 (splitUp, chop)

--http://codekata.com/kata/kata02-karate-chop/

splitUpTest :: Test
splitUpTest = TestList
  [ TestCase $ (Nothing) @=? splitUp 0 ([] :: [Int])
  , TestCase $ (Just (5, [], [])) @=? splitUp 0 ([5] :: [Int])
  , TestCase $ (Nothing) @=? splitUp 1 ([5] :: [Int])
  , TestCase $ (Just (3, [0..2], [4..9])) @=? splitUp 3 ([0..9] :: [Int])
  ]

chopTest :: Test
chopTest = TestList
  [ TestCase $ -1 @=? chop 3 ([] :: [Int])
  , TestCase $ -1 @=? chop 3 ([1] :: [Int])
  , TestCase $ 0 @=? chop 1 ([1] :: [Int])
  -- #
  , TestCase $ 0 @=? chop 1 ([1,3,5] :: [Int])
  , TestCase $ 1 @=? chop 3 ([1,3,5] :: [Int])
  , TestCase $ 2 @=? chop 5 ([1,3,5] :: [Int])
  , TestCase $ -1 @=? chop 0 ([1,3,5] :: [Int])
  , TestCase $ -1 @=? chop 2 ([1,3,5] :: [Int])
  , TestCase $ -1 @=? chop 4 ([1,3,5] :: [Int])
  , TestCase $ -1 @=? chop 6 ([1,3,5] :: [Int])
  -- #
  , TestCase $ 0 @=? chop 1 ([1,3,5,7] :: [Int])
  , TestCase $ 1 @=? chop 3 ([1,3,5,7] :: [Int])
  , TestCase $ 2 @=? chop 5 ([1,3,5,7] :: [Int])
  , TestCase $ 3 @=? chop 7 ([1,3,5,7] :: [Int])
  , TestCase $ -1 @=? chop 0 ([1,3,5,7] :: [Int])
  , TestCase $ -1 @=? chop 2 ([1,3,5,7] :: [Int])
  , TestCase $ -1 @=? chop 4 ([1,3,5,7] :: [Int])
  , TestCase $ -1 @=? chop 6 ([1,3,5,7] :: [Int])
  , TestCase $ -1 @=? chop 8 ([1,3,5,7] :: [Int])
  ]

main :: IO Counts
main = runTestTT $ TestList [splitUpTest, chopTest]
