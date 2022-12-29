module Types where

import Control.Monad.State

data Square = Number Int | Empty deriving (Show)
type Board = [Square]

type StateIO s a = StateT s IO a
