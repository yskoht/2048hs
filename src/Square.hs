module Square where

import Types
import Sample

isEmpty :: Square -> Bool
isEmpty Empty = True
isEmpty _ = False

createNumber :: IO Square
createNumber =
  sample [Number 2, Number 4]

label :: Square -> String
label Empty = "        "
label (Number n)
  | n < 10        = "    " ++ show n ++ "   "
  | n < 100       = "   " ++ show n ++ "   "
  | n < 1000      = "  " ++ show n ++ "   "
  | n < 10000     = "  " ++ show n ++ "  "
  | n < 100000    = " " ++ show n ++ "  "
  | n < 1000000   = " " ++ show n ++ " "
  | n < 10000000  = show n ++ " "
  | n < 100000000 = show n
  | otherwise = error ""


fmt :: Square -> [String]
fmt s = [
  "+--------+",
  "|        |",
  "|"++ss++"|",
  "|        |",
  "+--------+"]
  where ss = label s
