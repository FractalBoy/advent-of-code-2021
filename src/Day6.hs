module Day6 (part1, part2) where

import AOC
import Control.Monad
import Control.Monad.State

data FishMap = FishMap {day0 :: Int, day1 :: Int, day2 :: Int, day3 :: Int, day4 :: Int, day5 :: Int, day6 :: Int, day7 :: Int, day8 :: Int} deriving (Show)

part1 :: [String] -> String
part1 = show . totalFish . simulateDays 80 . parseFish . head

part2 :: [String] -> String
part2 = show . totalFish . simulateDays 256 . parseFish . head

totalFish :: FishMap -> Int
totalFish fm = day0 fm + day1 fm + day2 fm + day3 fm + day4 fm + day5 fm + day6 fm + day7 fm + day8 fm

simulateDays :: Int -> FishMap -> FishMap
simulateDays n = execState (replicateM n simulateDay)

simulateDay :: State FishMap ()
simulateDay = do
  fishMap <- get
  put
    FishMap
      { day0 = day1 fishMap,
        day1 = day2 fishMap,
        day2 = day3 fishMap,
        day3 = day4 fishMap,
        day4 = day5 fishMap,
        day5 = day6 fishMap,
        day6 = day7 fishMap + day0 fishMap,
        day7 = day8 fishMap,
        day8 = day0 fishMap
      }
  return ()

parseFish :: String -> FishMap
parseFish =
  foldl
    ( \acc f -> case f of
        "0" -> acc {day0 = day0 acc + 1}
        "1" -> acc {day1 = day1 acc + 1}
        "2" -> acc {day2 = day2 acc + 1}
        "3" -> acc {day3 = day3 acc + 1}
        "4" -> acc {day4 = day4 acc + 1}
        "5" -> acc {day5 = day5 acc + 1}
        "6" -> acc {day6 = day6 acc + 1}
        "7" -> acc {day7 = day7 acc + 1}
        "8" -> acc {day8 = day8 acc + 1}
        n -> undefined
    )
    (FishMap 0 0 0 0 0 0 0 0 0)
    . split ','
