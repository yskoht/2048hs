module Types where

import Control.Monad.State

data Square = Number Int | Empty deriving (Show, Eq)
type Board = [Square]

type StateIO s a = StateT s IO a

data Key = UpKey | DownKey | LeftKey | RightKey | QuitKey | UnknownKey deriving (Show)
