{-# OPTIONS_GHC -Wall #-}

module Day11 (solve) where

import           Data.Char (digitToInt)
import           Data.Map  (Map)
import qualified Data.Map               as M
import           Data.Set  (Set)
import qualified Data.Set               as S

type Coord  = (Int,Int)
type Coords = Set (Int,Int)
type Grid   = Map Coord Int

parse :: [[Int]] -> Grid
parse = M.fromList
      . concatMap (\(r, e) -> zipWith (\c d -> ((r,c), d)) [0..] e)
      . zip [0..]

adj :: Grid -> Coord -> Int -> Grid
adj g (r, c) _ = M.restrictKeys g . S.fromList 
               $ [ (r + 1, c + 0), (r - 1, c + 0)
                 , (r + 0, c + 1), (r + 0, c - 1)
                 , (r + 1, c + 1), (r + 1, c - 1)
                 , (r - 1, c + 1), (r - 1, c - 1) ]

(<+>) :: Int -> Int -> Int
(<+>) a b = min 10 $ a + b

flash :: Coords -> Grid -> Grid
flash f g | f' == f   = M.map (`mod` 10) g'
          | otherwise = flash k g'
  where
    c  = flip M.withoutKeys f $ M.filter (==10) g
    k  = S.union f $ M.keysSet c
    g' = M.unionWith (<+>) g . M.unionsWith (+)
       . map (M.map (const 1) . uncurry (adj g)) $ M.toList c
    f' = M.keysSet $ M.filter (==10) g'

step :: Int -> Grid -> Int
step 0 _ = 0
step n g = f' + step n' g'
  where
    n' = pred n
    g' = flash S.empty $ M.map succ g
    f' = M.size $ M.filter (==0) g'

sync :: Int -> Grid -> Int
sync n g | M.size z' == M.size g' = n'
         | otherwise              = sync n' g'
  where
    n' = succ n
    g' = flash S.empty $ M.map succ g
    z' = M.filter (==0) g'

solve :: IO ()
solve = do
    input <- parse . map (map digitToInt) . lines <$> readFile path
    let silver = step 100 input
        gold   = sync   0 input
    putStrLn $ "\tSilver: " ++ show silver
          ++ "\n\tGold:   " ++ show gold
  where
    path = "./rsc/day11.txt"
