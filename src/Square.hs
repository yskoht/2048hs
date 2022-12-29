module Square where

import Types
import Sample

isEmpty :: Square -> Bool
isEmpty Empty = True
isEmpty _ = False

createNumber :: IO Square
createNumber =
  sample [Number 2, Number 4]
