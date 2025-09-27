{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Day05 (solve) where

import           Data.Map             (Map)
import qualified Data.Map        as M
import           Data.List.Split      (splitOn)

type Coord = (Int, Int)

tt :: [a] -> (a,a)
tt (a : b : _) = (a,b)
tt _           = undefined

parse :: String -> (Coord, Coord)
parse = tt . map (tt . map (read @Int) . splitOn ",") . splitOn " -> "

range :: Int -> Int -> [Int]
range a b | a < b     = [a         .. b]
          | a > b     = [a, pred a .. b]
          | otherwise = [a]

straights :: [(Coord, Coord)] -> [Map Coord Int]
straights [] = []
straights (((x1, y1), (x2, y2)) : xs) 
    | (x1 == x2) || (y1 == y2) = cs : straights xs
    | otherwise                =      straights xs
  where
    cs  = M.fromList [ ((x,y), 1) | x <- range x1 x2, y <- range y1 y2 ]

diags :: [(Coord, Coord)] -> [Map Coord Int]
diags [] = []
diags (((x1, y1), (x2, y2)) : xs) 
    | (x1 == x2) || (y1 == y2) = cs  : diags xs
    | otherwise                = cs' : diags xs
  where
    xs' = range x1 x2
    ys' = range y1 y2
    cs  = M.fromList [ ((x,y), 1) | x <- xs', y <- ys' ] 
    cs' = M.fromList $ zipWith (curry (, 1)) xs' ys'

solve :: IO ()
solve = do
    input <- map parse . lines <$> readFile path
    let silver = reduce $ straights input
        gold   = reduce $ diags input
    putStrLn $ "\tSilver: " ++ show silver
          ++ "\n\tGold:   " ++ show gold
  where
    path   = "./rsc/day05.txt"
    reduce = M.size . M.filter (>1) . foldr1 (M.unionWith (+))
