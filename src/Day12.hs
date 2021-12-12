module Day12 (part1, part2) where

import AOC
import Control.Monad
import Data.Char
import qualified Data.Map as M

type CaveMap = M.Map String [String]

part1 :: [String] -> String
part1 = show . length . findPaths canVisitCave . parseInput

part2 :: [String] -> String
part2 = show . length . findPaths canVisitCaveModified . parseInput

findPaths :: (String -> [String] -> Bool) -> CaveMap -> [[String]]
findPaths canVisit caveMap = findPaths' [] "start"
  where
    findPaths' :: [String] -> String -> [[String]]
    findPaths' visited cave =
      let neighbors = M.lookup cave caveMap
          getNeighboringPaths = join $ maybe [] (map $ findPaths' (cave : visited)) neighbors
       in if cave == "end"
            then [reverse (cave : visited)]
            else
              if canVisit cave visited
                then getNeighboringPaths
                else []

canVisitCave :: String -> [String] -> Bool
canVisitCave cave visited = isBigCave cave || cave `notElem` visited

canVisitCaveModified :: String -> [String] -> Bool
canVisitCaveModified cave visited =
  isBigCave cave
    || cave == "start"
    && null visited
      || cave /= "start"
    && (cave `notElem` visited || not (haveVisitedSmallCaveTwice visited))

isBigCave :: String -> Bool
isBigCave = all isUpper

isSmallCave :: String -> Bool
isSmallCave = all isLower

haveVisitedSmallCaveTwice :: [String] -> Bool
haveVisitedSmallCaveTwice = not . null . M.filter (> 1) . foldl (\acc cave -> M.insertWith (+) cave 1 acc) M.empty . filter isSmallCave

parseInput :: [String] -> CaveMap
parseInput = foldl addToCaveMap M.empty . map ((\[a, b] -> (a, b)) . split '-')

addToCaveMap :: CaveMap -> (String, String) -> CaveMap
addToCaveMap m (pointA, pointB) =
  let m' = M.insertWith (++) pointB [pointA] m
   in M.insertWith (++) pointA [pointB] m'
