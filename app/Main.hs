module Main where

import qualified AOC (getInput)
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Data.Semigroup ((<>))
import qualified Day1 (part1, part2)
import GHC.IO.Handle.Text (hPutStrLn)
import Options.Applicative
import System.Environment
import System.IO (hPutStrLn, stderr)

data Options = Options
  { day :: Integer,
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
  input <- AOC.getInput day

  when (part1 || (not part1 && not part2)) $ showSolution day 1 input -- run part 1 when neither part specified
  when part2 $ showSolution day 2 input -- run part2 only when specified

showSolution :: Integer -> Integer -> String -> IO ()
showSolution day part input = void $
  runMaybeT $ do
    solution <- solve day part input
    lift $ putStrLn solution

solve :: Integer -> Integer -> String -> MaybeT IO String
solve 1 1 xs = MaybeT $ return $ Just $ Day1.part1 $ lines xs
solve 1 2 xs = MaybeT $ return $ Just $ Day1.part2 $ lines xs
solve day part _ = do
  lift $ hPutStrLn stderr $ "Day " ++ show day ++ " Part " ++ show part ++ " not yet implemented."
  MaybeT $ return Nothing
