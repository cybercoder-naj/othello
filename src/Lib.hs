module Lib where

import Types
import Control.Monad.State

initGameState :: GameState
initGameState = (N 0, B cells) where
  cells = replicate 3 (replicate 8 Empty) ++
    [replicate 3 Empty ++ [Black, White] ++ replicate 3 Empty] ++
    [replicate 3 Empty ++ [White, Black] ++ replicate 3 Empty] ++
    replicate 3 (replicate 8 Empty)

place :: (Int, Int) -> State GameState ()
place (x, y) = do 
  (N c, B cells) <- get
  
  pure ()