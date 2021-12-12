module Day8 (part1, part2) where

import AOC
import Control.Monad.State
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as Set

data InputOutput = InputOutput {input :: [String], output :: [String]} deriving (Show)

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

part2 :: [String] -> String
part2 = show . sum . decodeAllInputOutput . parseInput

decodeAllInputOutput :: [InputOutput] -> [Int]
decodeAllInputOutput = map decodeInputOutput

decodeInputOutput :: InputOutput -> Int
decodeInputOutput io = decodeOutput (decodeInput io) io

decodeOutput :: M.Map String Int -> InputOutput -> Int
decodeOutput decoder (InputOutput _ o) = read $ concatMap (\unknown -> show $ fromJust $ M.lookup (Set.toList $ Set.fromList unknown) decoder) o

decodeInput :: InputOutput -> M.Map String Int
decodeInput (InputOutput inp _) =
  let (one, four, seven, eight) = getTupleOfSet $ getKnownSegments inp
      unknowns = map Set.fromList $ getUnknownSegments inp
   in M.fromList $
        [(Set.toList one, 1), (Set.toList four, 4), (Set.toList seven, 7), (Set.toList eight, 8)]
          ++ map
            ( \unknown ->
                ( Set.toList unknown,
                  mapDifferencesToDigit
                    (getStringDifference one unknown)
                    (getStringDifference four unknown)
                    (getStringDifference seven unknown)
                )
            )
            unknowns

getKnownSegments :: [String] -> (String, String, String, String) -- 1, 4, 7, 8
getKnownSegments segs = let (one, four, seven, eight) = getAllKnownSegments segs in (head one, head four, head seven, head eight)

getUnknownSegments :: [String] -> [String]
getUnknownSegments = filter (\seg -> let len = length seg in len /= 2 && len /= 4 && len /= 3 && len /= 7)

getAllKnownSegments :: [String] -> ([String], [String], [String], [String])
getAllKnownSegments segs =
  ( filterByLength 2 segs,
    filterByLength 4 segs,
    filterByLength 3 segs,
    filterByLength 7 segs
  )

filterByLength :: Int -> [[a]] -> [[a]]
filterByLength len = filter (\x -> length x == len)

getStringDifference :: Set.Set Char -> Set.Set Char -> (Int, Int)
getStringDifference a b = (length $ Set.difference b a, length $ Set.difference a b)

getTupleOfSet :: (Ord a) => ([a], [a], [a], [a]) -> (Set.Set a, Set.Set a, Set.Set a, Set.Set a)
getTupleOfSet (a, b, c, d) = (Set.fromList a, Set.fromList b, Set.fromList c, Set.fromList d)

parseInput :: [String] -> [InputOutput]
parseInput = map parseLine

parseLine :: String -> InputOutput
parseLine = evalState $ do
  beforePipe <- words <$> consumeUntilSequence "|"
  afterPipe <- gets words

  return $ InputOutput beforePipe afterPipe
