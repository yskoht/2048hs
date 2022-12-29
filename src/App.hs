module App
  ( app
  ) where

import Control.Monad.State

import Board

app :: IO ()
app = do
  execStateT initBoard emptyBoard >>= showBoard
