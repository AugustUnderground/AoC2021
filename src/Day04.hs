{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TypeApplications #-}

module Day04 (solve) where

import Data.List.Split (splitOn)
import Data.Map (Map, (!?))
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S

type Board = Map Int (Int,Int)
type Marks = Set (Int, Int)

rows :: [Marks]
rows = [ S.fromList [(x,y) | y <- [0..4] ] | x <- [0..4]]

cols :: [Marks]
cols = [ S.fromList [(x,y) | x <- [0..4] ] | y <- [0..4]]

parse :: [String] -> [Board]
parse [] = []
parse ls = m : parse ls'
  where
    b   = map (map (read @Int) . words) $ takeWhile (/= "") ls
    m   = M.fromList [ ((b !! r) !! c, (r, c))
                     | r <- [ 0 .. 4 ], c <- [0 .. 4] ]
    ls' = drop 1 $ dropWhile (/= "") ls

mark :: Int -> Board -> Marks -> Marks
mark n b ms = lu $ b !? n
  where
    lu :: Maybe (Int, Int) -> Marks
    lu (Just m) = S.insert m ms
    lu Nothing  = ms

unmarked :: Board -> Marks -> Int
unmarked b ms = M.foldrWithKey add 0 b
  where
    add n m s | S.member m ms = s
              | otherwise     = s + n

winners :: [Marks] -> [Int]
winners ms = [ i | i <- [ 0 .. length ms - 1]
             ,  any (`S.isSubsetOf` (ms !! i) ) rows
             || any (`S.isSubsetOf` (ms !! i) ) cols ]

bingo :: [Int] -> [Board] -> [Marks] -> Int
bingo  []     _      _     = 0
bingo (n : ns) boards marks | null ws   = bingo ns boards marks'
                            | otherwise = n * unmarked b m
  where
    marks' = zipWith (mark n) boards marks
    ws     = winners marks'
    b      = boards !! head ws
    m      = marks' !! head ws

delAt :: Int -> [a] -> [a]
delAt i xs = let (f,b) = splitAt i xs in f ++ drop 1 b

bingo' :: [Int] -> [Board] -> [Marks] -> Int
bingo'  []     _      _     = 0
bingo' (n : ns) boards marks | null ws      = bingo' ns boards  marks'
                             | null boards' = n * unmarked b m
                             | otherwise    = bingo' ns boards' marks''
  where
    marks'  = zipWith (mark n) boards marks
    ws      = winners marks'
    boards' = foldr delAt boards ws
    marks'' = foldr delAt marks' ws
    b       = boards !! head ws
    m       = marks' !! head ws

solve :: IO ()
solve = do
    input <- lines <$> readFile path
    let nums   = map (read @Int) . splitOn "," $ head input
        boards = parse $ drop 2 input
        marks  = replicate (length boards) S.empty
    let silver = bingo  nums boards marks
        gold   = bingo' nums boards marks
    putStrLn $ "\tSilver: " ++ show silver
          ++ "\n\tGold:   " ++ show gold
  where
    path = "./rsc/day04.txt"
