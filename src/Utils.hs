module Utils where

import Types

isTarget :: Cell -> Bool
isTarget Target = True
isTarget _      = False 

isEmpty :: Cell -> Bool
isEmpty Empty = True
isEmpty _      = False 

even :: Counter -> Bool
even (N n) = Prelude.even n  

forEachCell :: (Int -> Int -> Cell -> a) -> [[Cell]] -> [[a]]
forEachCell f cells = [[f i j cell | (cell, j) <- zip row [0..]] | (row, i) <- zip cells [0..]]