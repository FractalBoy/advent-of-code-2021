module Day14 (part1, part2) where

import AOC
import Control.Monad.State
import Data.List
import qualified Data.Map as M
import Data.Maybe

type InsertionRules = M.Map String (String, String)

type PairCounts = M.Map String Int

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
part1 = solve 10

part2 :: [String] -> String
part2 = solve 40

solve :: Int -> [String] -> String
solve n input =
  let rules = getInsertionRules input
      template = getPolymerTemplate input
      counts = getPairCounts template
      charCounts = map snd $ countElements $ doInsertions n rules counts
   in show $ maximum charCounts - minimum charCounts

countElements :: PairCounts -> [(Char, Int)]
countElements counts =
  map
    ( \(char, count) ->
        let newCount = (if even count then count else count + 1)
         in (char, newCount `div` 2)
    )
    $ M.toList $
      foldl
        ( \acc (x : y : _, count) ->
            M.insertWith (+) y count (M.insertWith (+) x count acc)
        )
        M.empty
        $ M.toList counts

doInsertions :: Int -> InsertionRules -> PairCounts -> PairCounts
doInsertions 1 rules counts = doInsertion rules counts
doInsertions n rules counts = doInsertions (n - 1) rules (doInsertion rules counts)

doInsertion :: InsertionRules -> PairCounts -> PairCounts
doInsertion rules counts = foldl addPair M.empty $ M.keys counts
  where
    addPair :: PairCounts -> String -> PairCounts
    addPair acc pair =
      let amountToAdd = M.lookup pair counts
          whatToAdd = M.lookup pair rules
       in case amountToAdd of
            Nothing -> acc
            Just amountToAdd -> case whatToAdd of
              Nothing -> acc
              Just (firstToAdd, secondToAdd) ->
                M.insertWith
                  (+)
                  secondToAdd
                  amountToAdd
                  ( M.insertWith (+) firstToAdd amountToAdd acc
                  )

getPolymerTemplate :: [String] -> String
getPolymerTemplate = head

getInsertionRules :: [String] -> InsertionRules
getInsertionRules = M.fromList . map parseRule . drop 2

getPairCounts :: String -> PairCounts
getPairCounts = foldl (\acc pair -> M.insertWith (+) pair 1 acc) M.empty . windows 2

parseRule :: String -> (String, (String, String))
parseRule = evalState $ do
  pair <- consumeUntilSequence " -> "
  insertion <- get

  return (pair, (head pair : insertion, insertion ++ [last pair]))
