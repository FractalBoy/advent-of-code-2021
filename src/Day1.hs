module Day1 (part1, part2) where

import Control.Applicative

part1 :: [String] -> String
part1 = show . increases . pairs . ints

part2 :: [String] -> String
part2 = show . increases . pairs . sums . threes . ints

increases :: [(Integer, Integer)] -> Integer
increases = foldl (\acc (x, y) -> if y > x then acc + 1 else acc) 0

ints :: [String] -> [Integer]
ints = map read

pairs :: [a] -> [(a, a)]
pairs xs = getZipList $ (,) <$> ZipList xs <*> ZipList (tail xs)

threes :: [a] -> [(a, a, a)]
threes xs = getZipList $ (,,) <$> ZipList xs <*> ZipList (tail xs) <*> ZipList (tail $ drop 1 xs)

sums :: [(Integer, Integer, Integer)] -> [Integer]
sums = map (\(x, y, z) -> x + y + z)
