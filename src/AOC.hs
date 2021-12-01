{-# LANGUAGE OverloadedStrings #-}

module AOC (getInput) where

import Configuration.Dotenv
import Control.Monad (void)
import Data.ByteString.Internal (packChars)
import Data.ByteString.Lazy.Char8 (unpack)
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types.Header
import System.Environment (getEnv)

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
