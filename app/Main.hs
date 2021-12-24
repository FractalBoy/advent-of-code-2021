module Main where

import AOC
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Data.Semigroup ((<>))
import qualified Day1
import qualified Day10
import qualified Day11
import qualified Day12
import qualified Day13
import qualified Day14
import qualified Day15
import qualified Day16
import qualified Day17
import qualified Day2
import qualified Day3
import qualified Day4
import qualified Day5
import qualified Day6
import qualified Day7
import qualified Day8
import qualified Day9
import Options.Applicative
import System.IO (hPutStrLn, stderr)

data Options = Options
  { day :: Int,
    part1 :: Bool,
    part2 :: Bool
  }

parser :: Parser Options
parser =
  Options
    <$> option
      auto
      ( long "day"
          <> short 'd'
          <> metavar "DAY"
          <> help "Day of the challenge"
      )
    <*> switch
      ( long "part1"
          <> help "Run part 1"
      )
    <*> switch
      ( long "part2"
          <> help "Run part 2"
      )

opts =
  info
    (parser <**> helper)
    ( fullDesc
        <> progDesc "Run the Advent of Code solutions for 2021"
        <> header "advent-of-code-2021 - Advent of Code 2021 solutions"
    )

main :: IO ()
main = do
  (Options day part1 part2) <- execParser opts
  input <- getInput day

  when (part1 || (not part1 && not part2)) $ showSolution day 1 input -- run part 1 when neither part specified
  when part2 $ showSolution day 2 input -- run part2 only when specified

showSolution :: Int -> Int -> String -> IO ()
showSolution day part input = void $
  runMaybeT $ do
    solution <- solve day part (lines input)
    lift $ putStrLn solution

solve :: Int -> Int -> [String] -> MaybeT IO String
solve 1 1 xs = MaybeT $ return $ Just $ Day1.part1 xs
solve 1 2 xs = MaybeT $ return $ Just $ Day1.part2 xs
solve 2 1 xs = MaybeT $ return $ Just $ Day2.part1 xs
solve 2 2 xs = MaybeT $ return $ Just $ Day2.part2 xs
solve 3 1 xs = MaybeT $ return $ Just $ Day3.part1 xs
solve 3 2 xs = MaybeT $ return $ Just $ Day3.part2 xs
solve 4 1 xs = MaybeT $ return $ Day4.part1 xs
solve 4 2 xs = MaybeT $ return $ Day4.part2 xs
solve 5 1 xs = MaybeT $ return $ Just $ Day5.part1 xs
solve 5 2 xs = MaybeT $ return $ Just $ Day5.part2 xs
solve 6 1 xs = MaybeT $ return $ Just $ Day6.part1 xs
solve 6 2 xs = MaybeT $ return $ Just $ Day6.part2 xs
solve 7 1 xs = MaybeT $ return $ Just $ Day7.part1 xs
solve 7 2 xs = MaybeT $ return $ Just $ Day7.part2 xs
solve 8 1 xs = MaybeT $ return $ Just $ Day8.part1 xs
solve 8 2 xs = MaybeT $ return $ Just $ Day8.part2 xs
solve 9 1 xs = MaybeT $ return $ Just $ Day9.part1 xs
solve 9 2 xs = MaybeT $ return $ Just $ Day9.part2 xs
solve 10 1 xs = MaybeT $ return $ Just $ Day10.part1 xs
solve 10 2 xs = MaybeT $ return $ Just $ Day10.part2 xs
solve 11 1 xs = MaybeT $ return $ Just $ Day11.part1 xs
solve 11 2 xs = MaybeT $ return $ Just $ Day11.part2 xs
solve 12 1 xs = MaybeT $ return $ Just $ Day12.part1 xs
solve 12 2 xs = MaybeT $ return $ Just $ Day12.part2 xs
solve 13 1 xs = MaybeT $ return $ Just $ Day13.part1 xs
solve 13 2 xs = MaybeT $ return $ Just $ Day13.part2 xs
solve 14 1 xs = MaybeT $ return $ Just $ Day14.part1 xs
solve 14 2 xs = MaybeT $ return $ Just $ Day14.part2 xs
solve 15 1 xs = MaybeT $ return $ Just $ Day15.part1 xs
solve 15 2 xs = MaybeT $ return $ Just $ Day15.part2 xs
solve 16 1 xs = MaybeT $ return $ Just $ Day16.part1 xs
solve 16 2 xs = MaybeT $ return $ Just $ Day16.part2 xs
solve 17 1 xs = MaybeT $ Day17.part1 xs
solve 17 2 xs = MaybeT $ Day17.part2 xs
solve day part _ = do
  lift $ hPutStrLn stderr $ "Day " ++ show day ++ " Part " ++ show part ++ " not yet implemented."
  MaybeT $ return Nothing
