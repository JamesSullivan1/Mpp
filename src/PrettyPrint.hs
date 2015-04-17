module PrettyPrint where

{- Functions for making things pretty -}

-- Replace all instances of 'p' in s with 'q'
replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace _ _ [] = []
replace p q s 
    | take (length p) s == p    = q ++ (replace p q (drop (length p) s))
    | otherwise                 = (head s):(replace p q (tail s))

-- Replace all instances of '\n' with '\n\t' (indenting the string
--  evenly)
indent :: [Char] -> [Char]
indent s = ("\t") ++ replace "\n" "\n\t" s

-- Concatenate all of the given lists with the given list seperator.
catlist :: (Eq a) => [a] -> [[a]] -> [a]
catlist _   [] = []
catlist sep ss = foldr1 (\a b -> a ++ sep ++ b) ss

-- Show all elements of a list, seperating with a given seperator.
showlist :: (Show a, Eq a) => [Char] -> [a] -> [Char]
showlist sep [] = "[]" ++ sep
showlist sep ss = catlist sep $ map show ss

-- Clean up a string by eliminating newlines and tabs.
tidy :: (Show a) => a -> [Char]
tidy s = filter (\c -> not (c `elem` "\n\t") ) $ show s

