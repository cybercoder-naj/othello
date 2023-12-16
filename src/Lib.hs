module Lib where

import Types

initGameState :: RawState
initGameState = (N 0, B cells) where
  cells = replicate 3 (replicate 8 Empty) ++
    [replicate 3 Empty ++ [Black, White] ++ replicate 3 Empty] ++
    [replicate 3 Empty ++ [White, Black] ++ replicate 3 Empty] ++
    replicate 3 (replicate 8 Empty)
