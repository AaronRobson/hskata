module TestKata04 where

import Test.HUnit
import Kata04 (parseWeatherItem, WeatherItem(..), parseFootballItem, FootballItem(..))

--http://codekata.com/kata/kata04-data-munging/

parseWeatherItemTest :: Test
parseWeatherItemTest = TestList
  [ TestCase $ (WeatherItem 1 88 59) @=? parseWeatherItem "   1  88    59    74          53.8       0.00 F       280  9.6 270  17  1.6  93 23 1004.5"
  , TestCase $ (WeatherItem 9 86 32) @=? parseWeatherItem "   9  86    32*   59       6  61.5       0.00         240  7.6 220  12  6.0  78 46 1018.6"
  , TestCase $ (WeatherItem 26 97 64) @=? parseWeatherItem "  26  97*   64    81          70.4       0.00 H       050  5.1 200  12  4.0 107 45 1014.9"
  ]

parseFootballItemTest :: Test
parseFootballItemTest = TestList
  [ TestCase $ (FootballItem "Arsenal" 79 36) @=? parseFootballItem "    1. Arsenal         38    26   9   3    79  -  36    87"
  , TestCase $ (FootballItem "Aston Villa" 46 47) @=? parseFootballItem "    8. Aston_Villa     38    12  14  12    46  -  47    50"
  ]

main :: IO Counts
main = runTestTT $ TestList [parseWeatherItemTest, parseFootballItemTest]
