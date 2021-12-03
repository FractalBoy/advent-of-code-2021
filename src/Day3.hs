module Day3 (part1) where

import Control.Applicative
import Data.Bits
import Data.Char

part1 :: [String] -> String
part1 input = show $ getEpsilonRate input * getGammaRate input

getGammaRate :: [String] -> Int
getGammaRate = binaryToInt . getBinary . avgColumns

getEpsilonRate :: [String] -> Int
getEpsilonRate = binaryToInt . getInvertedBinary . avgColumns

intToFloat :: (Integral a) => a -> Float
intToFloat = fromIntegral

floatLength :: [a] -> Float
floatLength = fromIntegral . length

getColumns :: [String] -> [String]
getColumns = getZipList . traverse ZipList

avgColumns :: [String] -> [Float]
avgColumns c = map ((/ floatLength c) . sum) $ reverse $ foldl (\acc x -> map (intToFloat . digitToInt) x : acc) [] $ getColumns c

getBinary :: [Float] -> String
getBinary = map (intToDigit . round)

getInvertedBinary :: [Float] -> String
getInvertedBinary = map flipBit . getBinary
  where
    flipBit '0' = '1'
    flipBit '1' = '0'
    flipBit _ = undefined

binaryToInt :: String -> Int
binaryToInt bin = sum $ zipWith (*) [2 ^ x | x <- [0 ..]] (reverse $ map digitToInt bin)
