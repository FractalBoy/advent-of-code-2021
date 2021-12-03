module Day3 (part1, part2) where

import Control.Applicative
import Data.Char

type RateFunction = [String] -> Int

type BitCriteria = [String] -> String

part1 :: [String] -> String
part1 input = show $ getEpsilonRate input * getGammaRate input

part2 :: [String] -> String
part2 input = show $ getOxygenGeneratorRating input * getCarbonDioxideScrubberRating input

getRating' :: Int -> BitCriteria -> [String] -> String
getRating' _ _ [x] = x
getRating' n criteriaFunc xs = getRating' (n + 1) criteriaFunc $ filterReport criteriaFunc n xs

getRating :: BitCriteria -> [String] -> String
getRating = getRating' 0

getOxygenGeneratorRating :: [String] -> Int
getOxygenGeneratorRating = binaryToInt . getRating oxygenGeneratorBitCriteria

getCarbonDioxideScrubberRating :: [String] -> Int
getCarbonDioxideScrubberRating = binaryToInt . getRating carbonDioxideScrubberBitCriteria

filterReport :: BitCriteria -> Int -> [String] -> [String]
filterReport criteriaFunc n xs = filter (\x -> criteria !! n == x !! n) xs
  where
    criteria = criteriaFunc xs

oxygenGeneratorBitCriteria :: [String] -> String
oxygenGeneratorBitCriteria = bitCriteria (\x -> if x >= 0.5 then '1' else '0')

carbonDioxideScrubberBitCriteria :: [String] -> String
carbonDioxideScrubberBitCriteria = bitCriteria (\x -> if x >= 0.5 then '0' else '1')

bitCriteria :: (Float -> Char) -> [String] -> String
bitCriteria criteriaFunc = map criteriaFunc . avgColumns

getGammaRate :: RateFunction
getGammaRate = binaryToInt . oxygenGeneratorBitCriteria

getEpsilonRate :: RateFunction
getEpsilonRate = binaryToInt . carbonDioxideScrubberBitCriteria

intToFloat :: (Integral a) => a -> Float
intToFloat = fromIntegral

floatLength :: [a] -> Float
floatLength = fromIntegral . length

getColumns :: [String] -> [String]
getColumns = getZipList . traverse ZipList

avgColumns :: [String] -> [Float]
avgColumns c = map ((/ floatLength c) . sum) $ reverse $ foldl (\acc x -> map (intToFloat . digitToInt) x : acc) [] $ getColumns c

binaryToInt :: String -> Int
binaryToInt bin = sum $ zipWith (*) [2 ^ x | x <- [0 ..]] (reverse $ map digitToInt bin)
