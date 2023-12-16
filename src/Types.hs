module Types where

import Control.Monad.State (State)

data Cell = Black | White | Empty | Target

newtype Counter = N Int
  deriving Show
newtype Board   = B [[Cell]]

type RawState = (Counter, Board)

type GameState = State RawState

instance Show Cell where
  show Black  = "B"
  show White  = "W"
  show Empty  = "-"
  show Target = "*"

join :: String -> [Cell] -> String
join sep = foldl (\acc cell -> show cell ++ sep ++ acc) ""

instance Show Board where
  show (B cells) = concatMap ((++ "\n") . join " ") cells
