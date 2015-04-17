{- Defines the token position data type. -}

module ParserPos where

import Text.Regex.Posix
import Data.List.Split

{- Internal Representation of token positions -}
data Pos    = Pos Line Column   deriving (Eq)
data Line   = Line Int          deriving (Eq)
data Column = Column Int        deriving (Eq)

getPos :: String -> Pos
getPos s = _getPos (splitOn ":" (s =~ "([0-9]+:[0-9]+)" :: String)) where
    _getPos [s1,s2] = Pos (Line (read s1 :: Int)) (Column (read s2 :: Int))

getSourceLine :: Pos -> [String] -> String
getSourceLine _ [] = ""
getSourceLine (Pos (Line 1) (Column c)) (a:rst) =  
    "\n>   "++a++(getSourceLine (Pos (Line 0) (Column c)) rst)
getSourceLine (Pos (Line n) (Column c)) (a:rst)
    | abs (n-1) < 3
        = "\n    "++a++(getSourceLine (Pos (Line (n-1)) (Column c)) rst)
    | abs (n-1) < 4
        = "\n    ....."++(getSourceLine (Pos (Line (n-1)) (Column c)) rst)
    | otherwise
        = (getSourceLine (Pos (Line (n-1)) (Column c)) rst)

instance Show Pos where
    show (Pos l c)  = show l ++ ":" ++ show c
instance Show Line where
    show (Line n)   = show n
instance Show Column where
    show (Column n) = show n

