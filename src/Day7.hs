{-# LANGUAGE TupleSections #-}

module Day7 where

import AOC
import qualified Data.Map as M

part1 :: [String] -> String
part1 = solve id

part2 :: [String] -> String
part2 = solve (\x -> sum [0 .. x])

solve :: (Int -> Int) -> [String] -> String
solve rateFunction input =
  let crabs = parseCrabs $ head input
   in show $ minimum $ map (getFuelUsedForAllCrabs rateFunction crabs) [minPosition crabs .. maxPosition crabs]

parseCrabs :: String -> M.Map Int Int
parseCrabs = foldl (flip $ uncurry $ M.insertWith (+)) M.empty . map ((,1) . read) . split ','

minPosition :: M.Map Int Int -> Int
minPosition = minimum . M.keys

maxPosition :: M.Map Int Int -> Int
maxPosition = maximum . M.keys

getFuelUsedForAllCrabs :: (Int -> Int) -> M.Map Int Int -> Int -> Int
getFuelUsedForAllCrabs rateFunction crabs x = M.foldlWithKey (\acc pos count -> acc + count * getFuelUsedForOneCrab rateFunction x pos) 0 crabs

getFuelUsedForOneCrab :: (Int -> Int) -> Int -> Int -> Int
getFuelUsedForOneCrab rateFunction x0 x1
  | x0 == x1 = 0
  | otherwise = rateFunction (abs $ x0 - x1)
