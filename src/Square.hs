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
  | n < 10        = "    " ++ c n ++ show n ++ reset ++ "   "
  | n < 100       = "   " ++ c n ++ show n ++ reset ++ "   "
  | n < 1000      = "  " ++ c n ++ show n ++ reset ++ "   "
  | n < 10000     = "  " ++ c n ++ show n ++ reset ++ "  "
  | n < 100000    = " " ++ c n ++ show n ++ reset ++ "  "
  | n < 1000000   = " " ++ c n ++ show n ++ reset ++ " "
  | n < 10000000  = c n ++ show n ++ reset ++ " "
  | n < 100000000 = c n ++ show n ++ reset
  | otherwise = error ""


fmt :: Square -> [String]
fmt s = [
  "+--------+",
  "|        |",
  "|"++ss++"|",
  "|        |",
  "+--------+"]
  where ss = label s


reset :: String
reset = "\ESC[0m"

_c :: String
_c = "\ESC[38;5;"

c :: Int -> String
c 2      = _c ++ "15m"
c 4      = _c ++ "223m"
c 8      = _c ++ "216m"
c 16     = _c ++ "208m"
c 32     = _c ++ "202m"
c 64     = _c ++ "196m"
c 128    = _c ++ "229m"
c 256    = _c ++ "221m"
c 512    = _c ++ "220m"
c 1024   = _c ++ "179m"
c 2048   = _c ++ "178m"
c 4096   = _c ++ "120m"
c 8192   = _c ++ "121m"
c 16384  = _c ++ "122m"
c 32768  = _c ++ "123m"
c 66536  = _c ++ "124m"
c 131072 = _c ++ "125m"
c _      = c 2
