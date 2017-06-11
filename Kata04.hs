module Kata04 where

import Data.Function (on)
import Safe (minimumByNote) --cabal install safe

--http://codekata.com/kata/kata04-data-munging/

type WeekNumber = Int
type Temperature = Float

data WeatherItem = WeatherItem { weekNumber :: WeekNumber
                               , maxTemperature :: Temperature
                               , minTemperature :: Temperature
                               } deriving (Show, Eq)
type WeatherTable = [WeatherItem]

parseWeatherItem :: String -> WeatherItem
parseWeatherItem line = WeatherItem weekNo maxTemp minTemp
    where
      items = words line
      weekNo = read (items !! 0) :: WeekNumber
      maxTemp = read (validate (items !! 1)) :: Temperature
      minTemp = read (validate (items !! 2)) :: Temperature
      validate :: String -> String
      validate = filter (/= '*')

parseWeatherTable :: [String] -> WeatherTable
parseWeatherTable tableLines = map parseWeatherItem dataLines
    where
      withoutHeader = drop 1 tableLines
      dataLines = filter (not . null) withoutHeader

temperatureSpread :: WeatherItem -> Temperature
temperatureSpread (WeatherItem _ maxTemp minTemp) = largest - smallest
    where
      largest = max maxTemp minTemp
      smallest = min maxTemp minTemp

minimumTemperatureSpread :: WeatherTable -> WeatherItem
minimumTemperatureSpread xs = minimumByNote "There are no items in the WeatherTable." (compare `on` temperatureSpread) xs

part1 :: IO ()
part1 = do
    putStrLn "Part 1"
    fileContents <- readFile "weather.dat"
    let weatherTable = parseWeatherTable $ lines fileContents
    let weekNoWithSmallestRange = weekNumber $ minimumTemperatureSpread weatherTable
    putStrLn $ "The week number with the smallest spread is " ++ (show weekNoWithSmallestRange) ++ ".\n"

main :: IO ()
main = putStrLn "CodeKata04:\n" >> part1
