module Main where

import qualified AOC (getInput)
import Data.Semigroup ((<>))
import qualified Day1 (part1, part2)
import Options.Applicative
import System.Environment

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
  let part' = partNumber part1 part2
  case part' of
    Nothing -> return ()
    Just part -> putStrLn $ solve day part input

partNumber :: Bool -> Bool -> Maybe Integer
partNumber False False = Just 1 -- Default to running part 1
partNumber True False = Just 1
partNumber False True = Just 2
partNumber _ _ = Nothing

solve :: Integer -> Integer -> String -> String
solve 1 1 = Day1.part1 . lines
solve 1 2 = Day1.part2 . lines
