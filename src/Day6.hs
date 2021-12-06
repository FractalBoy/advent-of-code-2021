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
  fish <- get
  put $ take 6 (tail fish) ++ [head fish + fish !! 7] ++ [fish !! 8, head fish]
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
