module Day11 (part1) where

import AOC (NumberGrid, getNumberGrid)
import Control.Monad.State
import Data.Bifunctor
import qualified Data.Map as M

example =
  [ "5483143223",
    "2745854711",
    "5264556173",
    "6141336146",
    "6357385478",
    "4167524645",
    "2176841721",
    "6882881134",
    "4846848554",
    "5283751526"
  ]

part1 :: [String] -> String
part1 = show . simulateSteps 100 . getNumberGrid

simulateSteps :: Int -> NumberGrid -> Int
simulateSteps n grid =
  let (flashed, _) = runState (replicateM n performStep) (grid, [])
   in sum flashed

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
        (grid, visited) <- get
        let grid' = M.adjust (+ 1) adj grid
        put (grid', visited)
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
