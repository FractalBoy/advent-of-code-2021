module Day15 where

import AOC
import Control.Monad
import Data.Foldable
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as Set

data Position = Position (Int, Int) Int deriving (Show)

type AdjacencyList = M.Map (Int, Int) [Position]

data Infinite a = Infinity | Finite a

instance Show a => Show (Infinite a) where
  show Infinity = "Infinity"
  show (Finite n) = show n

instance Eq a => Eq (Infinite a) where
  Infinity == Infinity = True
  (Finite a) == (Finite b) = a == b
  Infinity == (Finite _) = False
  (Finite _) == Infinity = False
  a /= b = not (a == b)

instance Ord a => Ord (Infinite a) where
  Infinity `compare` Infinity = EQ
  Infinity `compare` (Finite _) = GT
  (Finite _) `compare` Infinity = LT
  (Finite a) `compare` (Finite b) = a `compare` b

instance Functor Infinite where
  fmap _ Infinity = Infinity
  fmap fab (Finite a) = Finite (fab a)

instance Applicative Infinite where
  pure = Finite
  fs <*> as = do
    f <- fs
    f <$> as

instance Monad Infinite where
  return = Finite
  x >>= y = case x of
    Infinity -> Infinity
    Finite x -> y x

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
      distances = M.fromList [(coord, Infinity) | coord <- M.keys graph]
      distances' = M.adjust (const $ Finite 0) source distances
   in findOptimalPath' source target (M.keys graph) distances'
  where
    findOptimalPath' :: (Int, Int) -> (Int, Int) -> [(Int, Int)] -> M.Map (Int, Int) (Infinite Int) -> Infinite Int
    findOptimalPath' source target queue distances =
      if null queue
        then fromMaybe Infinity $ M.lookup target distances
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
                            Just (Position coord cost) -> do
                              let alt = dist >>= (\x -> Finite (x + cost))
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
