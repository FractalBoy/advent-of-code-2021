module Day11 (part1, part2) where

import AOC (NumberGrid, getNumberGrid)
import Control.Monad.State
import Data.Bifunctor
import qualified Data.Map as M

part1 :: [String] -> String
part1 = show . simulateSteps 100 . getNumberGrid

part2 :: [String] -> String
part2 = show . simulateUntilAllFlash . getNumberGrid

simulateUntilAllFlash :: NumberGrid -> Int
simulateUntilAllFlash grid = evalState (performStepAndCheckIfAllFlashing 0) (grid, [])

performStepAndCheckIfAllFlashing :: Int -> State (NumberGrid, [(Int, Int)]) Int
performStepAndCheckIfAllFlashing step = do
  performStep
  (grid, _) <- get

  if allFlashing grid
    then return $ step + 1
    else performStepAndCheckIfAllFlashing $ step + 1

allFlashing :: NumberGrid -> Bool
allFlashing = all ((== 0) . snd) . M.toList

simulateSteps :: Int -> NumberGrid -> Int
simulateSteps n grid = sum $ evalState (replicateM n performStep) (grid, [])

performStep :: State (NumberGrid, [(Int, Int)]) Int
performStep = do
  (grid, _) <- get
  let grid' = M.map (+ 1) grid
  put (grid', [])
  void flashAppropriateOctopi
  (grid'', flashed) <- get
  put (M.mapWithKey (\k v -> if k `elem` flashed then 0 else v) grid'', [])
  return $ length flashed

increaseAdjacentEnergyLevels :: (Int, Int) -> State (NumberGrid, [(Int, Int)]) ()
increaseAdjacentEnergyLevels coord = do
  mapM_
    ( \adj -> do
        (grid, flashed) <- get
        let grid' = M.adjust (+ 1) adj grid
        put (grid', flashed)
        return ()
    )
    $ getAdjacentOctopi coord

flashAppropriateOctopi :: State (NumberGrid, [(Int, Int)]) ()
flashAppropriateOctopi = do
  (grid, _) <- get

  mapM_ flashOctopus $
    filter
      ( \coord ->
          maybe False (> 9) (M.lookup coord grid)
      )
      $ M.keys grid

flashOctopus :: (Int, Int) -> State (NumberGrid, [(Int, Int)]) ()
flashOctopus coord@(x, y) = do
  (grid, flashed) <- get

  when (coord `notElem` flashed) $ do
    let grid' = M.adjust (const 0) coord grid
    put (grid', coord : flashed)

    increaseAdjacentEnergyLevels coord
    flashAppropriateOctopi

getAdjacentOctopi :: (Int, Int) -> [(Int, Int)]
getAdjacentOctopi (y, x) =
  filter (\(y, x) -> y >= 0 && x >= 0 && y <= 9 && x <= 9) $
    map (bimap (y +) (x +)) $
      filter
        (/= (0, 0))
        $ (,)
          <$> [1, 0, -1]
          <*> [1, 0, -1]
