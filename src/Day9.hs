module Day9 (part1, part2) where

import AOC (getNumberGrid)
import Control.Monad
import Data.Char
import Data.List
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as Set

type HeightMap = M.Map (Int, Int) Int

part1 :: [String] -> String
part1 input =
  let hm = getNumberGrid input
   in show $ sum $ map ((+ 1) . fromMaybe 0 . (($ hm) . M.lookup)) $ getLowPoints hm

part2 :: [String] -> String
part2 input =
  let hm = getNumberGrid input
   in show $ product $ take 3 $ reverse $ sort $ map (length . getBasinFromLowPoint hm) $ getLowPoints hm

getNeighbors :: (Int, Int) -> [(Int, Int)]
getNeighbors (y, x) = [(y - 1, x), (y, x - 1), (y + 1, x), (y, x + 1)]

getLowPoints :: HeightMap -> [(Int, Int)]
getLowPoints m = M.keys $ M.filterWithKey (\k _ -> isLowPoint m k) m

isLowPoint :: HeightMap -> (Int, Int) -> Bool
isLowPoint m (y, x) =
  let neighbors = map (($ m) . M.lookup) $ getNeighbors (y, x)
      point = M.lookup (y, x) m
   in all
        (fromMaybe True . (\neighbor -> (<) <$> point <*> neighbor))
        neighbors

getBasinFromLowPoint :: HeightMap -> (Int, Int) -> [(Int, Int)]
getBasinFromLowPoint m lowPoint = Set.toList $ getBasinFromLowPoint' m (Set.fromList [lowPoint]) Set.empty
  where
    getBasinFromLowPoint' m visit visited =
      case Set.toList visit of
        [] -> visited
        (current : remaining) ->
          if Set.member current visited
            then getBasinFromLowPoint' m (Set.fromList remaining) visited
            else
              let visited' = Set.insert current visited
                  unvisitedNeighbors =
                    filter
                      ( \neighbor ->
                          let value = M.lookup neighbor m
                           in (isJust value && value /= Just 9)
                      )
                      $ getNeighbors current
               in getBasinFromLowPoint' m (Set.fromList $ remaining ++ unvisitedNeighbors) visited'
