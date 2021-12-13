module Day13 (part1, part2) where

import AOC
import Control.Monad
import qualified Data.Set as Set

data Axis = X | Y

part1 :: [String] -> String
part1 input =
  let coords = getCoordinates input
      folds = getFolds input
   in show $ length $ doFold coords (head folds)

part2 :: [String] -> String
part2 input =
  let coords = getCoordinates input
      folds = getFolds input
   in drawGrid $ foldl doFold coords folds

drawGrid :: [(Int, Int)] -> String
drawGrid points =
  let xs = map fst points
      ys = map snd points
      minX = minimum xs
      minY = minimum ys
      maxX = maximum xs
      maxY = maximum ys
      allPoints = (,) <$> [minY .. maxY] <*> [minX .. maxX]
   in join $
        map
          ( \(y, x) ->
              let newLine = (if x == maxX then ['\n'] else [' '])
               in if (x, y) `elem` points then '#' : newLine else '.' : newLine
          )
          allPoints

doFold :: [(Int, Int)] -> (Axis, Int) -> [(Int, Int)]
doFold points fold@(X, value) = (Set.toList . Set.fromList) $ filter ((< value) . fst) points ++ map (reflectPoint fold) (filter ((> value) . fst) points)
doFold points fold@(Y, value) = (Set.toList . Set.fromList) $ filter ((< value) . snd) points ++ map (reflectPoint fold) (filter ((> value) . snd) points)

reflectPoint :: (Axis, Int) -> (Int, Int) -> (Int, Int)
reflectPoint (Y, value) (x, y) = (x, y - ((y - value) * 2))
reflectPoint (X, value) (x, y) = (x - ((x - value) * 2), y)

getFolds :: [String] -> [(Axis, Int)]
getFolds =
  map
    ( ( \[axis, value] -> case axis of
          "fold along x" -> (X, read value)
          "fold along y" -> (Y, read value)
          _ -> undefined
      )
        . split '='
    )
    . tail
    . dropWhile (not . null)

getCoordinates :: [String] -> [(Int, Int)]
getCoordinates = map ((\[y, x] -> (read y, read x)) . split ',') . takeWhile (not . null)
