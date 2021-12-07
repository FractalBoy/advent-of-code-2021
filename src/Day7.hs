{-# LANGUAGE TupleSections #-}

module Day7 where

import AOC
import Data.List
import qualified Data.Map as M

part1 :: [String] -> String
part1 = solve id

part2 :: [String] -> String
part2 = solve (\x -> sum [0 .. x])

solve :: (Int -> Int) -> [String] -> String
solve _ [] = undefined
solve rateFunction (str : _) =
  let crabs = parseCrabs str
   in show $ minimum $ map (getFuelUsed rateFunction crabs) [minPosition crabs .. maxPosition crabs]

parseCrabs :: String -> M.Map Int Int
parseCrabs str = foldl (flip $ uncurry $ M.insertWith (+)) M.empty $ map ((,1) . read) $ split ',' str

minPosition :: M.Map Int Int -> Int
minPosition = minimum . M.keys

maxPosition :: M.Map Int Int -> Int
maxPosition = maximum . M.keys

getFuelUsed :: (Int -> Int) -> M.Map Int Int -> Int -> Int
getFuelUsed rateFunction crabs x =
  let lessThan = M.filterWithKey (\k _ -> k < x) crabs
      greaterThan = M.filterWithKey (\k _ -> k > x) crabs
      equal = M.filterWithKey (\k _ -> k == x) crabs
   in sum (map (\(pos, count) -> rateFunction (x - pos) * count) $ M.toList lessThan) + sum (map (\(pos, count) -> rateFunction (pos - x) * count) $ M.toList greaterThan)
