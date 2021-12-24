module Day17 (part1, part2) where

import Debug.Trace
import Debug.Trace (traceShowId)
import Text.Regex.PCRE.String

part1 :: [String] -> IO (Maybe String)
part1 input = do
  targetArea <- getTargetArea $ head input

  case targetArea of
    Nothing -> return Nothing
    Just (_, rangeY) -> return $ Just $ show $ maximum $ simulateYVelocities rangeY

part2 :: [String] -> IO (Maybe String)
part2 input = do
  targetArea <- getTargetArea $ head input

  case targetArea of
    Nothing -> return Nothing
    Just range -> return $ Just $ show $ length $ getInitialVelocities range

getTargetArea :: String -> IO (Maybe ((Int, Int), (Int, Int)))
getTargetArea input = do
  result <- compile compBlank execBlank "target area: x=(\\d+)\\.\\.(\\d+), y=(-\\d+)\\.\\.(-\\d+)"

  case result of
    Left _ -> return Nothing
    Right regex -> do
      result <- regexec regex input
      case result of
        Left error -> return Nothing
        Right result -> case result of
          Just (_, _, _, [xMinStr, xMaxStr, yMinStr, yMaxStr]) -> return $ Just ((read xMinStr, read xMaxStr), (read yMinStr, read yMaxStr))
          _ -> return Nothing

getInitialVelocities :: ((Int, Int), (Int, Int)) -> [(Int, Int)]
getInitialVelocities range@(rangeX@(xMin, xMax), rangeY@(yMin, yMax)) =
  let allPossibleVelocities = (,) <$> [0 .. xMax] <*> validYVelocities rangeY
   in map fst $
        filter (\(_, trajectory) -> any (\(posX, posY) -> posX >= xMin && posX <= xMax && posY >= yMin && posY <= yMax) trajectory) $
          map (\initial@(velX, velY) -> (initial, simulateTrajectory range ((0, velX), (0, velY)))) allPossibleVelocities

simulateTrajectory :: ((Int, Int), (Int, Int)) -> ((Int, Int), (Int, Int)) -> [(Int, Int)]
simulateTrajectory range@(rangeX@(xMin, xMax), rangeY@(yMin, yMax)) (pointX@(positionX, velocityX), pointY@(positionY, velocityY)) =
  let positionX' = positionX + velocityX
      velocityX'
        | velocityX > 0 = velocityX - 1
        | velocityX < 0 = velocityX + 1
        | otherwise = 0
      positionY' = positionY + velocityY
      velocityY' = velocityY - 1
   in if velocityY' < 0 && positionY' < yMin || positionX' > xMax
        then [(positionX, positionY)]
        else (positionX, positionY) : simulateTrajectory range ((positionX', velocityX'), (positionY', velocityY'))

simulateYVelocities :: (Int, Int) -> [Int]
simulateYVelocities range@(yMin, yMax) =
  map
    (\(_, trajectory) -> maximum $ map fst trajectory)
    $ filter
      (\(_, trajectory) -> any (\(pos, vel) -> pos >= yMin && pos <= yMax) trajectory)
      $ map
        (\y -> (y, simulateYTrajectory (0, y) range))
        [yMax * 2 .. (- yMax) * 2]

validYVelocities :: (Int, Int) -> [Int]
validYVelocities range@(yMin, yMax) =
  map fst $
    filter
      (\(_, trajectory) -> any (\(pos, vel) -> pos >= yMin && pos <= yMax) trajectory)
      $ map
        (\y -> (y, simulateYTrajectory (0, y) range))
        [yMax * 2 .. (- yMax) * 2]

simulateYTrajectory :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
simulateYTrajectory point@(position, velocity) range@(yMin, yMax) =
  let position' = position + velocity
      velocity' = velocity - 1
   in if velocity' < 0 && position' < yMin
        then [point]
        else point : simulateYTrajectory (position', velocity') range
