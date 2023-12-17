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