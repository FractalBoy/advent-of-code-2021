{-# LANGUAGE OverloadedStrings #-}

module AOC
  ( getInput,
    consumeUntilSequence,
    split,
    getNumberGrid,
    NumberGrid,
  )
where

import Configuration.Dotenv
import Control.Monad
import Control.Monad.State.Lazy
import Data.ByteString.Internal (packChars)
import Data.ByteString.Lazy.Char8 (unpack)
import Data.Char
import Data.List
import qualified Data.Map as M
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types.Header
import System.Environment (getEnv)

type NumberGrid = M.Map (Int, Int) Int

getInput :: Int -> IO String
getInput day = do
  manager <- newManager tlsManagerSettings

  let url = "https://adventofcode.com/2021/day/" ++ show day ++ "/input"
  request' <- parseRequest url

  void $ loadFile defaultConfig
  session <- getEnv "SESSION_ID"
  let sessionCookie = "session=" ++ session

  let request = request' {requestHeaders = (hCookie, packChars sessionCookie) : requestHeaders request'}

  response <- httpLbs request manager
  return $ unpack $ responseBody response

consumeUntilSequence :: (Eq a) => [a] -> State [a] [a]
consumeUntilSequence stopWord = do
  s <- get

  let (start, end) = execState consumeWhile' ([], s)

  put end
  return start
  where
    consumeOne =
      state
        ( \(start, end) -> case end of
            (x : xs) -> (Just x, (start ++ [x], xs))
            _ -> (Nothing, (start, end))
        )
    consumeWhile' = do
      c <- consumeOne
      (start, end) <- get

      case c of
        Just c ->
          if stopWord `isSuffixOf` start
            then put (take (length start - length stopWord) start, end)
            else consumeWhile'
        Nothing -> return ()

      return ()

split :: (Eq a) => a -> [a] -> [[a]]
split _ [] = []
split delim string =
  let (before, after) = span (/= delim) string
   in before : split delim (drop 1 after)

getNumberGrid :: [String] -> NumberGrid
getNumberGrid =
  M.fromList
    . join
    . zipWith (\y str -> zipWith (\x c -> ((y, x), digitToInt c)) [0 ..] str) [0 ..]
