module Kata04 where

import Data.Function (on)
import Safe (minimumByNote) --cabal install safe

--http://codekata.com/kata/kata04-data-munging/

--Part01

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
    weekNoWithSmallestRange <- fmap (weekNumber . minimumTemperatureSpread . parseWeatherTable . lines) $ readFile "weather.dat"
    putStrLn $ "The week number with the smallest spread is " ++ (show weekNoWithSmallestRange) ++ ".\n"

--Part02
type Goals = Int

data FootballItem = FootballItem { teamName :: String
                                 , goalsFor :: Goals
                                 , goalsAgainst :: Goals
                                 } deriving (Show, Eq)
type FootballTable = [FootballItem]

parseFootballItem :: String -> FootballItem
parseFootballItem line = FootballItem teamName goalsFor goalsAgainst
    where
      items = words line
      teamName = validateTeamName $ items !! 1
      goalsFor = read (items !! 6) :: Goals
      goalsAgainst = read (items !! 8) :: Goals
      validateTeamName :: String -> String
      validateTeamName = map (\x -> case x of '_' -> ' '
                                              _ -> x)

parseFootballTable :: [String] -> FootballTable
parseFootballTable tableLines = map parseFootballItem dataLines
    where
      withoutHeader = drop 1 tableLines
      isAllowedLine :: String -> Bool
      isAllowedLine = not . null . filter (/= ' ') . filter (/= '-')
      dataLines = filter isAllowedLine withoutHeader

goalSpread :: FootballItem -> Goals
goalSpread (FootballItem _ goalsFor goalsAgainst) = abs $ goalsFor - goalsAgainst

evenestGoals :: FootballTable -> FootballItem
evenestGoals = minimumByNote "There are no items in the FootballTable." (compare `on` goalSpread)

part2 :: IO ()
part2 = do
    putStrLn "Part 2"
    teamWithEvenestGoals <- fmap (teamName . evenestGoals . parseFootballTable . lines) $ readFile "football.dat"
    putStrLn $ "The team with the evenest goals is " ++ (show teamWithEvenestGoals) ++ ".\n"

main :: IO ()
main = putStrLn "CodeKata04:\n" >> part1 >> part2
