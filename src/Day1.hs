module Day1 (part1, part2) where

import AOC

part1 :: [String] -> String
part1 = show . increases . windows 2 . ints

part2 :: [String] -> String
part2 = show . increases . windows 2 . sums . windows 3 . ints

increases :: [[Int]] -> Int
increases = foldl (\acc (x : y : _) -> if y > x then acc + 1 else acc) 0

ints :: [String] -> [Int]
ints = map read

sums :: [[Int]] -> [Int]
sums = map sum
