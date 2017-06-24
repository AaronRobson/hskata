module TestKata06 where

import Test.HUnit
import Kata06 (whiteSpaceToSpace, validate, validateCheck, signature, areAnagrams, areAnagramsList)

--http://codekata.com/kata/kata06-anagrams/

whiteSpaceToSpaceTest :: Test
whiteSpaceToSpaceTest = TestList
  [ TestCase $ 'a' @=? whiteSpaceToSpace 'a'
  , TestCase $ 'A' @=? whiteSpaceToSpace 'A'
  , TestCase $ ' ' @=? whiteSpaceToSpace ' '
  , TestCase $ ' ' @=? whiteSpaceToSpace '\n'
  , TestCase $ ' ' @=? whiteSpaceToSpace '\r'
  , TestCase $ ' ' @=? whiteSpaceToSpace '\t'
  ]

validateTest :: Test
validateTest = TestList
  [ TestCase $ "" @=? validate ""
  , TestCase $ "a" @=? validate "a"
  , TestCase $ "a" @=? validate "A"
  , TestCase $ "a     b" @=? validate " A \n\r\t B "
  ]

validateCheckTest :: Test
validateCheckTest = TestList
  [ TestCase $ Just "" @=? validateCheck ""
  , TestCase $ Nothing @=? validateCheck "'"
  , TestCase $ Just "" @=? validateCheck " "
  , TestCase $ Just "a" @=? validateCheck "a"
  , TestCase $ Just "a" @=? validateCheck "A"
  , TestCase $ Just "ab" @=? validateCheck "ab"
  , TestCase $ Just "ba" @=? validateCheck "ba"
  , TestCase $ Just "a b" @=? validateCheck " A B "
  , TestCase $ Just "a     b" @=? validateCheck " A \n\r\t B "
  , TestCase $ Nothing @=? validateCheck " A \n B , ^&*<> C "
  ]

signatureTest :: Test
signatureTest = TestList
  [ TestCase $ "" @=? signature ""
  , TestCase $ "" @=? signature "'"
  , TestCase $ "" @=? signature " "
  , TestCase $ "a" @=? signature "a"
  , TestCase $ "a" @=? signature "A"
  , TestCase $ "ab" @=? signature "ab"
  , TestCase $ "ab" @=? signature "ba"
  , TestCase $ "ab" @=? signature " A \n\r\t B "
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
main = runTestTT $ TestList
    [ whiteSpaceToSpaceTest
    , validateTest
    , validateCheckTest
    , signatureTest
    , areAnagramsTest
    , areAnagramsListTest
    ]
