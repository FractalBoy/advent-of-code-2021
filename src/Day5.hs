{-# LANGUAGE TupleSections #-}

module Day5 (part1) where

import AOC
import Control.Applicative
import Control.Monad.State.Lazy
import Data.List
import qualified Data.Map as M

data Point = Point Int Int deriving (Show, Eq)

data Range = Range {start :: Point, end :: Point} deriving (Show)

instance Ord Point where
  (Point x0 y0) `compare` (Point x1 y1)
    | y0 > y1 = GT
    | y0 < y1 = LT
    | otherwise = x0 `compare` x1

part1 :: [String] -> String
part1 = show . length . getOverlappingPoints . getCoveredPoints

getOverlappingPoints :: [Point] -> [(Point, Int)]
getOverlappingPoints = filter ((> 1) . snd) . M.toList . foldl (flip (uncurry (M.insertWith (+)))) M.empty . map (,1)

getCoveredPoints :: [String] -> [Point]
getCoveredPoints = join . fillInRanges . parseRanges

fillInRanges :: [Range] -> [[Point]]
fillInRanges = map fillInRange

fillInRange :: Range -> [Point]
fillInRange (Range (Point x0 y0) (Point x1 y1))
  | x0 == x1 && y0 < y1 = getZipList $ Point <$> ZipList (repeat x0) <*> ZipList [y0 .. y1]
  | x0 == x1 && y0 > y1 = getZipList $ Point <$> ZipList (repeat x0) <*> ZipList [y1 .. y0]
  | y0 == y1 && x0 < x1 = getZipList $ Point <$> ZipList [x0 .. x1] <*> ZipList (repeat y0)
  | y0 == y1 && x0 > x1 = getZipList $ Point <$> ZipList [x1 .. x0] <*> ZipList (repeat y0)
  | otherwise = []

parseRanges :: [String] -> [Range]
parseRanges = map parseRange

parseRange :: String -> Range
parseRange =
  evalState $
    do
      p0 <- consumePoint
      p1 <- consumePoint
      return $ Range {start = p0, end = p1}

consumePoint :: State String Point
consumePoint = do
  x <- read <$> consumeUntilSequence ","
  y <- read <$> consumeUntilSequence " -> "
  return $ Point x y
