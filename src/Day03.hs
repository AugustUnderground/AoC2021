{-# OPTIONS_GHC -Wall #-}

module Day03 (solve) where

import Data.Char (digitToInt)
import Data.List (transpose)

b2d :: [Int] -> Int
b2d = foldl (\a x -> 2 * a + x) 0

rating :: [Int] -> Int
rating xs | null xs                   = 0
          | (sum xs * 2) >= length xs = 1
          | otherwise                 = 0

power :: [Int] -> [[Int]] -> Int
power gs [] = es' * gs'
  where
    es' = b2d . map (1-) $ reverse gs 
    gs' = b2d $ reverse gs
power gs (b : bs) = power (g : gs) bs
  where
    g = rating b

rating' :: Int -> Int -> [[Int]] -> Int
rating' _ _ [ ] = undefined
rating' _ _ [b] = b2d b
rating' o i bs  = rating' o (i + 1) b'
  where
    o' = abs . (o-) . rating . (!! i) $ transpose bs
    b' = filter ((==o') . (!! i)) bs

solve :: IO ()
solve = do
    input <- map (map digitToInt) . lines <$> readFile path
    let silver = power [] $ transpose input
        gold   = product [rating' o 0 input | o <- [0, 1]]
    putStrLn $ "\tSilver: " ++ show silver
          ++ "\n\tGold:   " ++ show gold
  where
    path = "./rsc/day03.txt"
