module GetKey
  ( getKey,
  ) where

import Data.Char

import Types

getKey :: IO Key
getKey = do
  c <- getChar
  case ord c of
    27 -> getKey
    91 -> getKey
    65 -> return UpKey -- 27 29 65 -> Up
    66 -> return DownKey -- 27 29 66 -> Down
    67 -> return RightKey -- 27 29 67 -> Right
    68 -> return LeftKey -- 27 29 68 -> Left
    104 -> return LeftKey -- h
    106 -> return DownKey -- j
    107 -> return UpKey -- k
    108 -> return RightKey -- l
    113 -> return QuitKey -- q
    _ -> return UnknownKey
