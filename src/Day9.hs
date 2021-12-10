module Day9 (part1) where

import Control.Monad
import Data.Char
import qualified Data.Map as M
import qualified Data.Maybe

part1 :: [String] -> String
part1 = show . sum . map (+ 1) . getLowPoints

getHeightMap :: [String] -> M.Map (Int, Int) Int
getHeightMap = M.fromList . join . zipWith (\y str -> zipWith (\x c -> ((y, x), digitToInt c)) [0 ..] str) [0 ..]

getNeighbors :: (Int, Int) -> [(Int, Int)]
getNeighbors (y, x) = [(y - 1, x), (y, x - 1), (y + 1, x), (y, x + 1)]

getLowPoints :: [String] -> [Int]
getLowPoints input =
  let map = getHeightMap input
   in M.elems $ M.filterWithKey (\k _ -> isLowPoint map k) map

isLowPoint :: M.Map (Int, Int) Int -> (Int, Int) -> Bool
isLowPoint m (y, x) =
  let neighbors = map (($ m) . M.lookup) $ getNeighbors (y, x)
      point = M.lookup (y, x) m
   in all
        (Data.Maybe.fromMaybe True . (\neighbor -> (<) <$> point <*> neighbor))
        neighbors
