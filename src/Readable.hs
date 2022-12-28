module Readable where

import Text.Printf
import Types

fmt :: Square -> String
fmt Empty = replicate 8 ' '
fmt (Number n) = printf "  %4d  " n

readable :: Board -> [String]
readable board = map fmt board
