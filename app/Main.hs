module Main where

import qualified Lib
import qualified Types

main :: IO ()
main = printBoard Lib.initGameState

printBoard :: Types.RawState -> IO ()
printBoard (_, board) = print board