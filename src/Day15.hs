module Day15 (part1, part2) where

import AOC
import Control.Monad
import qualified Data.Map as M
import Data.Maybe
import qualified Data.PQueue.Prio.Min as Q

data Position = Position (Int, Int) Float deriving (Show)

type AdjacencyList = M.Map (Int, Int) [Position]

part1 :: [String] -> String
part1 = show . floor . findOptimalPath . numberGridToAdjacencyList . getNumberGrid

part2 :: [String] -> String
part2 = show . floor . findOptimalPath . numberGridToAdjacencyList . expandNumberGrid . getNumberGrid

findOptimalPath :: AdjacencyList -> Float
findOptimalPath graph =
  let distances = M.fromList [(coord, if coord == source then 0 else 1 / 0) | coord <- M.keys graph]
   in findOptimalPath' (Q.fromList [(0, source)]) distances
  where
    source = (0, 0)
    target = maximum $ M.keys graph
    findOptimalPath' :: Q.MinPQueue Float (Int, Int) -> M.Map (Int, Int) Float -> Float
    findOptimalPath' queue distances =
      if Q.null queue
        then fromMaybe (1 / 0) (M.lookup target distances)
        else
          let ((dist, closest), queue') = Q.deleteFindMin queue
           in case M.lookup closest distances of
                Nothing -> 1 / 0
                Just currDist ->
                  if currDist == dist
                    then
                      let neighbors = fromMaybe [] (M.lookup closest graph)
                          newCosts =
                            mapMaybe
                              ( \(Position coord cost) ->
                                  let alt = dist + cost
                                   in case M.lookup coord distances of
                                        Nothing -> Nothing
                                        Just oldCost -> if alt < oldCost then Just (alt, coord) else Nothing
                              )
                              neighbors
                          queue'' = Q.union queue' (Q.fromList newCosts)
                          distances' = foldl (\acc (cost, coord) -> M.insert coord cost acc) distances newCosts
                       in findOptimalPath' queue'' distances'
                    else findOptimalPath' queue' distances

numberGridToAdjacencyList :: NumberGrid -> AdjacencyList
numberGridToAdjacencyList grid =
  M.fromList $
    map
      ( \coord ->
          ( coord,
            mapMaybe
              ( \neighbor ->
                  let risk = M.lookup neighbor grid
                   in case risk of
                        Nothing -> Nothing
                        Just risk -> Just (Position neighbor (fromIntegral risk))
              )
              $ getNeighbors coord
          )
      )
      $ M.keys grid

expandNumberGrid :: NumberGrid -> NumberGrid
expandNumberGrid grid =
  let width = (+ 1) $ maximum $ map snd $ M.keys grid
      height = (+ 1) $ maximum $ map fst $ M.keys grid
   in M.fromList $
        join $
          mapMaybe
            ( \coord@(y, x) ->
                let originalValue = M.lookup coord grid
                 in case originalValue of
                      Nothing -> Nothing
                      Just originalValue ->
                        Just $
                          ( \m n ->
                              ( (y + m * width, x + n * height),
                                let newValue = originalValue + m + n
                                 in if newValue > 9 then newValue - 9 else newValue
                              )
                          )
                            <$> [0 .. 4]
                            <*> [0 .. 4]
            )
            $ M.keys grid

getNeighbors :: (Int, Int) -> [(Int, Int)]
getNeighbors (y, x) = [(y - 1, x), (y, x - 1), (y + 1, x), (y, x + 1)]
