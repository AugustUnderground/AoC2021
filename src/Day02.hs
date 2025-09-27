{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TypeApplications #-}

module Day02 (solve) where

import Data.List (isPrefixOf)

type Pos = (Int, Int)
type Pos' = (Int, Int, Int)

step :: Pos -> [String] -> Int
step (x,y)   []                               = x * y
step (x,y) (s : c) | "forward" `isPrefixOf` s = step (x + d, y) c
                   | "down"    `isPrefixOf` s = step (x, y + d) c
                   | "up"      `isPrefixOf` s = step (x, y - d) c
                   | otherwise                = step (x, y) c
  where
    d = read @Int $ words s !! 1

step' :: Pos' -> [String] -> Int
step' (x,y,_)   []                               = x * y
step' (x,y,a) (s : c) | "forward" `isPrefixOf` s = step' (x+d, y+(a*d), a) c
                      | "down"    `isPrefixOf` s = step' (x, y, a+d) c
                      | "up"      `isPrefixOf` s = step' (x, y, a-d) c
                      | otherwise                = step' (x, y, a) c
  where
    d = read @Int $ words s !! 1

solve :: IO ()
solve = do
    input <- lines <$> readFile path
    let silver = step  (0,0)   input
        gold   = step' (0,0,0) input
    putStrLn $ "\tSilver: " ++ show silver
          ++ "\n\tGold:   " ++ show gold
  where
    path = "./rsc/day02.txt"
