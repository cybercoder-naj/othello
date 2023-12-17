{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types where

data Cell = Black | White | Empty | Target
  deriving Eq

newtype Counter = N Int
  deriving (Show, Eq, Ord, Num)

newtype Board   = B [[Cell]]

type GameState = (Counter, Board)

instance Show Cell where
  show Black  = "B"
  show White  = "W"
  show Empty  = "-"
  show Target = "*"