{-# OPTIONS_GHC -Wall #-}

module Day10 (solve) where

import Data.List (sort)

pointTable :: Char -> Int
pointTable ')' = 3
pointTable ']' = 57
pointTable '}' = 1197
pointTable '>' = 25137
pointTable  _  = 0

pointTable' :: Char -> Int
pointTable' ')' = 1
pointTable' ']' = 2
pointTable' '}' = 3
pointTable' '>' = 4
pointTable'  _  = 0

close :: Char -> Char
close '(' = ')'
close '[' = ']'
close '{' = '}'
close '<' = '>'
close  _  = ' '

isOpen :: Char -> Bool
isOpen '(' = True
isOpen '[' = True
isOpen '{' = True
isOpen '<' = True
isOpen  _  = False

check :: String -> String -> Int
check (s : ss) (x : xs) | isOpen x  = check (close x : s : ss) xs
                        | s  ==  x  = check ss xs
                        | otherwise = pointTable x
check      ""  (x : xs)             = check [close x] xs
check    _        _                 = 0

complete :: String -> String -> Int
complete (s : ss) (x : xs) | isOpen x  = complete (close x : s : ss) xs
                           | s  ==  x  = complete ss xs
                           | otherwise = 0
complete      ""  (x : xs)             = complete [close x] xs
complete      ss       ""              = foldl (\a s -> a * 5 + pointTable' s) 0 ss

mid :: [a] -> a
mid l@(_:_:_:_) = mid $ tail $ init l
mid l           = head l

solve :: IO ()
solve = do
    input <- lines <$> readFile path
    let silver = sum $ map (check "") input
        gold   = mid . sort . filter (>0) $ map (complete "") input
    putStrLn $ "\tSilver: " ++ show silver
          ++ "\n\tGold:   " ++ show gold
  where
    path = "./rsc/day10.txt"
