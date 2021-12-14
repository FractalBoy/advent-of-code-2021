module Day14 (part1) where

import AOC
import Control.Monad.State
import Data.List
import qualified Data.Map as M
import Data.Maybe

type InsertionRules = M.Map String String

example =
  [ "NNCB",
    "",
    "CH -> B",
    "HH -> N",
    "CB -> H",
    "NH -> C",
    "HB -> C",
    "HC -> B",
    "HN -> C",
    "NN -> C",
    "BH -> H",
    "NC -> B",
    "NB -> B",
    "BN -> B",
    "BB -> N",
    "BC -> B",
    "CC -> N",
    "CN -> C"
  ]

part1 :: [String] -> String
part1 input =
  let template = getPolymerTemplate input
      rules = getInsertionRules input
      polymer = countElements $ doInsertions rules 10 template
      counts = map snd polymer
   in show $ maximum counts - minimum counts

countElements :: String -> [(Char, Int)]
countElements = M.toList . foldl (\acc c -> M.insertWith (+) c 1 acc) M.empty

doInsertions :: InsertionRules -> Int -> String -> String
doInsertions rules 1 template = doInsertion rules template
doInsertions rules n template = doInsertions rules (n - 1) (doInsertion rules template)

doInsertion :: InsertionRules -> String -> String
doInsertion rules template = foldl (\acc pair -> acc ++ head pair : fromMaybe "" (M.lookup pair rules)) "" (windows 2 template) ++ [last template]

getPolymerTemplate :: [String] -> String
getPolymerTemplate = head

getInsertionRules :: [String] -> InsertionRules
getInsertionRules = M.fromList . map parseRule . drop 2

parseRule :: String -> (String, String)
parseRule = evalState $ do
  pair <- consumeUntilSequence " -> "
  insertion <- get

  return (pair, insertion)
