module Day16 (part1) where

import AOC
import Control.Monad.State
import Data.Maybe

data Packet = LiteralPacket Int Int Int | OperatorPacket Int Int [Packet] deriving (Show)

part1 :: [String] -> String
part1 input = fromMaybe "" $ do
  let binary = convertHexToBinary $ head input
  packet <- evalState consumePacket binary
  return $ show $ sumVersions packet

sumVersions :: Packet -> Int
sumVersions (LiteralPacket version _ _) = version
sumVersions (OperatorPacket version _ packets) = version + sum (map sumVersions packets)

splitPacket :: String -> (Int, Int, String)
splitPacket packet = ((binaryToInt . take 3) packet, (binaryToInt . take 3 . drop 3) packet, drop 6 packet)

convertHexToBinary :: String -> String
convertHexToBinary = concatMap hexDigitToBinary

consumePacket :: State String (Maybe Packet)
consumePacket = do
  packet <- get
  if null packet
    then return Nothing
    else
      let (_, typeId, _) = splitPacket packet
       in if typeId == 4
            then Just <$> consumeLiteralPacket
            else Just <$> consumeOperatorPacket

consumePacketsOfLength :: Int -> State String [Packet]
consumePacketsOfLength length = do
  packet <- get
  let (parsable, unparsable) = splitAt length packet
  put parsable
  results <- catMaybes . takeWhile isJust <$> sequence (repeat consumePacket)
  put unparsable
  return results

consumePacketsOfNumber :: Int -> State String [Packet]
consumePacketsOfNumber number = catMaybes <$> replicateM number consumePacket

consumeOperatorPacket :: State String Packet
consumeOperatorPacket = do
  packet <- get
  let (version, typeId, remainder) = splitPacket packet
  if head remainder == '0'
    then do
      let totalLength = (binaryToInt . take 15 . drop 1) remainder
          subpackets = drop 16 remainder
      put subpackets
      OperatorPacket version typeId <$> consumePacketsOfLength totalLength
    else do
      let numSubPackets = (binaryToInt . take 11 . drop 1) remainder
          subpackets = drop 12 remainder
      put subpackets
      OperatorPacket version typeId <$> consumePacketsOfNumber numSubPackets

consumeLiteralPacket :: State String Packet
consumeLiteralPacket = do
  packet <- get
  let (version, typeId, remainder) = splitPacket packet

  let (parsed, remainder') = consumeLiteralPacket' remainder
   in do
        put remainder'
        return $ LiteralPacket version typeId (binaryToInt parsed)
  where
    consumeLiteralPacket' (a : b : c : d : e : packet) =
      if a == '1'
        then
          let (parsed, remainder) = consumeLiteralPacket' packet
           in (b : c : d : e : parsed, remainder)
        else (b : c : d : e : "", packet)
    consumeLiteralPacket' [] = ("", "")
    consumeLiteralPacket' packet = ("", packet)

hexDigitToBinary :: Char -> String
hexDigitToBinary '0' = "0000"
hexDigitToBinary '1' = "0001"
hexDigitToBinary '2' = "0010"
hexDigitToBinary '3' = "0011"
hexDigitToBinary '4' = "0100"
hexDigitToBinary '5' = "0101"
hexDigitToBinary '6' = "0110"
hexDigitToBinary '7' = "0111"
hexDigitToBinary '8' = "1000"
hexDigitToBinary '9' = "1001"
hexDigitToBinary 'A' = "1010"
hexDigitToBinary 'B' = "1011"
hexDigitToBinary 'C' = "1100"
hexDigitToBinary 'D' = "1101"
hexDigitToBinary 'E' = "1110"
hexDigitToBinary 'F' = "1111"
hexDigitToBinary _ = ""
