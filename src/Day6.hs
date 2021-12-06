module Day6 (part1, part2) where

import AOC
import Control.Monad
import Control.Monad.State

part1 :: [String] -> String
part1 = show . sum . simulateDays 80 . parseFish . head

part2 :: [String] -> String
part2 = show . sum . simulateDays 256 . parseFish . head

simulateDays :: Int -> [Int] -> [Int]
simulateDays n = execState (replicateM n simulateDay)

simulateDay :: State [Int] ()
simulateDay = do
  fishMap <- get
  put $ take 6 (tail fishMap) ++ [head fishMap + fishMap !! 7] ++ [fishMap !! 8, head fishMap]
  return ()

parseFish :: String -> [Int]
parseFish =
  foldl
    ( \acc f ->
        let (x, y) = splitAt f acc
         in case y of
              (y : ys) -> x ++ y + 1 : ys
              _ -> x
    )
    (replicate 9 0)
    . map read
    . split ','
