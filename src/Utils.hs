module Utils where

import Types

isTarget :: Cell -> Bool
isTarget Target = True
isTarget _      = False 

even :: Counter -> Bool
even (N n) = Prelude.even n  