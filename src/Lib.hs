module Lib where

import Types
import qualified Utils
import Control.Monad.State
import Control.Monad.Writer
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

  unless (Utils.isTarget $ cells !! x !! y) $ return ()

  put (N (c + 1), B (execState (putPlayer x y (player c) (opponent c)) cells))
  modify findTargets
  pure ()

  where
    player c = if Prelude.even c then Black else White
    opponent c = if Prelude.even c then White else Black

putPlayer :: Int -> Int -> Cell -> Cell -> State [[Cell]] ()
putPlayer x y pl op = do
  modify (changeCell x y pl)
  forM_ directions
    (\(dx, dy) -> do
      cells <- get
      let (build, turnovers) = runWriter (buildCellsToTurn (x + dx) (y + dy) (dx, dy) pl op cells)
      when build $ turnPieces turnovers pl
    )

turnPieces :: [(Int, Int)] -> Cell -> State [[Cell]] ()
turnPieces turnovers pl = modify (Utils.forEachCell (\i j cell ->
    if (i, j) `elem` turnovers then
      pl
    else
      cell
    )
  )

buildCellsToTurn :: Int -> Int -> (Int, Int) -> Cell -> Cell -> [[Cell]] -> Writer [(Int, Int)] Bool
buildCellsToTurn x y (dx, dy) pl op cells = do
  case getCell x y cells of
    Just cell
      | cell == op -> tell [(x, y)] >> buildCellsToTurn (x + dx) (y + dy) (dx, dy) pl op cells
      | cell == pl -> return True
      | Utils.isEmpty cell -> return False
      | otherwise       -> return False
    Nothing -> return False

changeCell :: Int -> Int -> Cell -> [[Cell]] -> [[Cell]]
changeCell x y pl =
  Utils.forEachCell (\i j cell ->
    if x == i && y == j then
      pl
    else if Utils.isTarget cell then
      Empty
    else cell
  )

findTargets :: GameState -> GameState
findTargets (c, B cells) = (c, B mappedCells)
  where
    mappedCells :: [[Cell]]
    mappedCells =
      Utils.forEachCell (\i j cell ->
        if Utils.isEmpty cell && any (target (i, j)) directions then
          Target
        else
          cell
      ) cells

    opponent = if Utils.even c then White else Black
    player   = if Utils.even c then Black else White

    target :: (Int, Int) -> (Int, Int) -> Bool
    target = (isJust .) . go
      where
        go :: (Int, Int) -> (Int, Int) -> Maybe Cell
        go (x, y) (dx, dy) = do
          adj <- getCell (x + dx) (y + dy) cells
          cell <- getCell (x + 2 * dx) (y + 2 * dy) cells
          if adj == opponent then
            if cell == player then
              return Target
            else if Utils.isEmpty cell then
              Nothing
            else go (x + dx, y + dy) (dx, dy)
          else Nothing

directions :: [(Int, Int)]
directions = [ (1, 0),(0, 1), (0, -1), (-1, 0), (1, 1), (-1, -1), (1, -1), (-1, 1)]

getCell :: Int -> Int -> [[Cell]] -> Maybe Cell
getCell i j cells
  | i < 0 || j < 0 || i >= 8 || j >= 8 = Nothing
  | otherwise                          = Just (cells !! i !! j)