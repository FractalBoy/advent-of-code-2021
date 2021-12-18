module Day15 where

import AOC
import Control.Monad
import Data.Foldable
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as Set

data Position = Position (Int, Int) Int deriving (Show)

type AdjacencyList = M.Map (Int, Int) [Position]

data Infinite a = PositiveInfinity | NegativeInfinity | Finite a deriving (Eq)

instance Show a => Show (Infinite a) where
  show PositiveInfinity = "Infinity"
  show NegativeInfinity = "-Infinity"
  show (Finite n) = show n

instance Ord a => Ord (Infinite a) where
  PositiveInfinity `compare` PositiveInfinity = EQ
  PositiveInfinity `compare` NegativeInfinity = GT
  NegativeInfinity `compare` PositiveInfinity = LT
  NegativeInfinity `compare` NegativeInfinity = EQ
  PositiveInfinity `compare` (Finite _) = GT
  NegativeInfinity `compare` (Finite _) = LT
  (Finite _) `compare` PositiveInfinity = LT
  (Finite _) `compare` NegativeInfinity = GT
  (Finite a) `compare` (Finite b) = a `compare` b

instance Num a => Num (Infinite a) where
  PositiveInfinity + PositiveInfinity = PositiveInfinity
  PositiveInfinity + NegativeInfinity = undefined
  NegativeInfinity + PositiveInfinity = undefined
  NegativeInfinity + NegativeInfinity = NegativeInfinity
  PositiveInfinity + (Finite _) = PositiveInfinity
  NegativeInfinity + (Finite _) = NegativeInfinity
  (Finite _) + PositiveInfinity = PositiveInfinity
  (Finite _) + NegativeInfinity = NegativeInfinity
  (Finite a) + (Finite b) = Finite (a + b)
  PositiveInfinity * PositiveInfinity = undefined
  PositiveInfinity * NegativeInfinity = PositiveInfinity
  NegativeInfinity * PositiveInfinity = NegativeInfinity
  NegativeInfinity * NegativeInfinity = undefined
  PositiveInfinity * (Finite _) = PositiveInfinity
  NegativeInfinity * (Finite _) = NegativeInfinity
  (Finite _) * PositiveInfinity = NegativeInfinity
  (Finite _) * NegativeInfinity = PositiveInfinity
  (Finite a) * (Finite b) = Finite (a * b)
  abs PositiveInfinity = PositiveInfinity
  abs NegativeInfinity = PositiveInfinity
  abs (Finite a) = Finite (abs a)
  signum PositiveInfinity = Finite 1
  signum NegativeInfinity = Finite (-1)
  signum (Finite a) = Finite (signum a)
  negate PositiveInfinity = NegativeInfinity
  negate NegativeInfinity = PositiveInfinity
  negate (Finite a) = Finite (negate a)
  fromInteger a = Finite (fromInteger a)

example =
  [ "1163751742",
    "1381373672",
    "2136511328",
    "3694931569",
    "7463417111",
    "1319128137",
    "1359912421",
    "3125421639",
    "1293138521",
    "2311944581"
  ]

part1 :: [String] -> String
part1 = show . findOptimalPath . numberGridToAdjacencyList . getNumberGrid

findOptimalPath :: AdjacencyList -> Infinite Int
findOptimalPath graph =
  let source = (0, 0)
      target = (maximum $ map fst $ M.keys graph, maximum $ map snd $ M.keys graph)
      distances = M.fromList [(coord, PositiveInfinity) | coord <- M.keys graph]
      distances' = M.adjust (const $ Finite 0) source distances
   in findOptimalPath' source target (M.keys graph) distances'
  where
    findOptimalPath' :: (Int, Int) -> (Int, Int) -> [(Int, Int)] -> M.Map (Int, Int) (Infinite Int) -> Infinite Int
    findOptimalPath' source target queue distances =
      if null queue
        then fromMaybe PositiveInfinity $ M.lookup target distances
        else
          let (closest, dist) =
                minimumBy (\(_, a) (_, b) -> a `compare` b) $
                  mapMaybe
                    ( \coord -> do
                        dist <- M.lookup coord distances
                        return (coord, dist)
                    )
                    queue
              queue' = filter (/= closest) queue
              neighbors = filter (\(Position coord _) -> coord `elem` queue') $ fromMaybe [] (M.lookup closest graph)
              distances' =
                M.mapMaybeWithKey
                  ( \k v ->
                      let neighbor = find (\(Position coord _) -> coord == k) neighbors
                       in case neighbor of
                            Nothing -> return v
                            Just (Position _ cost) -> do
                              let alt = dist + fromInteger (toInteger cost)
                               in if alt < v then return alt else return v
                  )
                  distances
           in findOptimalPath' source target queue' distances'

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
                        Just risk -> Just (Position neighbor risk)
              )
              $ getNeighbors coord
          )
      )
      $ M.keys grid

getNeighbors :: (Int, Int) -> [(Int, Int)]
getNeighbors (y, x) = [(y - 1, x), (y, x - 1), (y + 1, x), (y, x + 1)]
