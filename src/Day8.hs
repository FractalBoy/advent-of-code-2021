{-# LANGUAGE LambdaCase #-}

module Day8 (part1, part2) where

import AOC
import Control.Monad.State
import Data.List (find)
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as Set

type Signal = String

type Wire = Char

type InputSignal = [Signal]

type OutputSignal = [Signal]

data InputOutput = InputOutput {input :: InputSignal, output :: OutputSignal} deriving (Show)

mapDifferencesToDigit :: (Int, Int) -> (Int, Int) -> (Int, Int) -> Int
mapDifferencesToDigit (4, 0) (3, 1) (3, 0) = 0
mapDifferencesToDigit (0, 0) _ _ = 1
mapDifferencesToDigit (4, 1) (3, 2) (3, 1) = 2
mapDifferencesToDigit (3, 0) (2, 1) (2, 0) = 3
mapDifferencesToDigit _ (0, 0) _ = 4
mapDifferencesToDigit (4, 1) (2, 1) (3, 1) = 5
mapDifferencesToDigit (5, 1) (3, 1) (4, 1) = 6
mapDifferencesToDigit _ _ (0, 0) = 7
mapDifferencesToDigit (0, _) (0, _) (0, _) = 8
mapDifferencesToDigit (4, 0) (2, 0) (3, 0) = 9
mapDifferencesToDigit _ _ _ = undefined

part1 :: [String] -> String
part1 = show . foldl (\acc (one, four, seven, eight) -> acc + sum (map length [one, four, seven, eight])) 0 . map (getAllKnownSegments . output) . parseInput

part2 :: IO String
part2 = do
  --  input <- parseInput . lines <$> readFile "/home/mreisner/advent-of-code-2021/sample.txt"
  let i = parseLine "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf"
  let (one, four, seven, eight) = getTupleOfSet $ getKnownSegments $ input i
  let unknown = map Set.fromList $ getUnknownSegments $ input i
  let differences =
        map
          ( \u ->
              ( u,
                ( (length $ Set.difference u one, length $ Set.difference one u),
                  (length $ Set.difference u four, length $ Set.difference four u),
                  (length $ Set.difference u seven, length $ Set.difference seven u)
                )
              )
          )
          unknown

  let y = map (\(letter, (one, four, seven)) -> (letter, mapDifferencesToDigit one four seven)) differences

  print y
  return ""

getKnownSegments :: [String] -> (String, String, String, String) -- 1, 4, 7, 8
getKnownSegments segs = let (one, four, seven, eight) = getAllKnownSegments segs in (head one, head four, head seven, head eight)

getUnknownSegments :: [String] -> [String]
getUnknownSegments = filter (\seg -> let len = length seg in len /= 2 && len /= 4 && len /= 3 && len /= 7)

getAllKnownSegments :: [String] -> ([String], [String], [String], [String])
getAllKnownSegments segs = (filter (\seg -> length seg == 2) segs, filter (\seg -> length seg == 4) segs, filter (\seg -> length seg == 3) segs, filter (\seg -> length seg == 7) segs)

getStringDifference :: String -> String -> (Int, Int)
getStringDifference a b =
  let setA = Set.fromList a
      setB = Set.fromList b
   in (length $ Set.difference setB setA, length $ Set.difference setA setB)

getTupleOfSet :: (Ord a) => ([a], [a], [a], [a]) -> (Set.Set a, Set.Set a, Set.Set a, Set.Set a)
getTupleOfSet (a, b, c, d) = (Set.fromList a, Set.fromList b, Set.fromList c, Set.fromList d)

parseInput :: [String] -> [InputOutput]
parseInput = map parseLine

parseLine :: String -> InputOutput
parseLine = evalState $ do
  beforePipe <- words <$> consumeUntilSequence "|"
  afterPipe <- gets words

  return $ InputOutput beforePipe afterPipe
