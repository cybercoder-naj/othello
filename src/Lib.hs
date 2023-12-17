module Lib where

import Types
import qualified Utils
import Control.Monad.State
import Data.Maybe (isJust)

initGameState :: GameState
initGameState = findTargets (N 0, B cells)
  where
    cells = replicate 3 (replicate 8 Empty) ++
      [replicate 3 Empty ++ [Black, White] ++ replicate 3 Empty] ++
      [replicate 3 Empty ++ [White, Black] ++ replicate 3 Empty] ++
      replicate 3 (replicate 8 Empty)

place :: (Int, Int) -> State GameState ()
place (x, y) = do
  (N c, B cells) <- get

  if not . Utils.isTarget $ cells !! x !! y then
    return ()
  else do
    put (N (c + 1), putPlayer x y c cells)
    modify findTargets
  pure ()

  where
    putPlayer :: Int -> Int -> Int -> [[Cell]] -> Board
    putPlayer x y pl cells = B [[if x == i && y == j then player pl else if Utils.isTarget cell then Empty else cell | (cell, j) <- zip row [0..]] | (row, i) <- zip cells [0..]]

    player c = if Prelude.even c then Black else White

findTargets :: GameState -> GameState
findTargets (c, B cells) = (c, B mappedCells)
  where
    mappedCells :: [[Cell]]
    mappedCells = [[if Utils.isEmpty cell && any (target (i, j)) directions then Target else cell | (cell, j) <- zip row [0..]] | (row, i) <- zip cells [0..]]

    directions = [(0, 1), (0, -1), (1, 0), (-1, 0), (1, 1), (-1, -1), (1, -1), (-1, 1)]

    getCell :: Int -> Int -> Maybe Cell
    getCell i j
      | i < 0 || j < 0 || i >= 8 || j >= 8 = Nothing
      | otherwise                          = Just (cells !! i !! j)

    opponent = if Utils.even c then White else Black
    player   = if Utils.even c then Black else White

    target :: (Int, Int) -> (Int, Int) -> Bool
    target = (isJust .) . go
      where
        go :: (Int, Int) -> (Int, Int) -> Maybe Cell
        go (x, y) (dx, dy) = do
          adj <- getCell (x + dx) (y + dy)
          cell <- getCell (x + 2 * dx) (y + 2 * dy)
          if adj == opponent then
            if cell == player then
              return Target
            else if Utils.isEmpty cell then
              Nothing
            else go (x + dx, y + dy) (dx, dy)
          else Nothing