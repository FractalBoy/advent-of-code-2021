module Main where

import AOC
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Data.Semigroup ((<>))
import qualified Day1
import qualified Day2
import qualified Day3
import qualified Day4
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
    solution <- solve day part input
    lift $ putStrLn solution

solve :: Int -> Int -> String -> MaybeT IO String
solve 1 1 xs = MaybeT $ return $ Just $ Day1.part1 $ lines xs
solve 1 2 xs = MaybeT $ return $ Just $ Day1.part2 $ lines xs
solve 2 1 xs = MaybeT $ return $ Just $ Day2.part1 $ lines xs
solve 2 2 xs = MaybeT $ return $ Just $ Day2.part2 $ lines xs
solve 3 1 xs = MaybeT $ return $ Just $ Day3.part1 $ lines xs
solve 3 2 xs = MaybeT $ return $ Just $ Day3.part2 $ lines xs
solve 4 1 xs = MaybeT $ return $ Day4.part1 $ lines xs
solve day part _ = do
  lift $ hPutStrLn stderr $ "Day " ++ show day ++ " Part " ++ show part ++ " not yet implemented."
  MaybeT $ return Nothing
