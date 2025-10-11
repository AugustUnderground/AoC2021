{-# OPTIONS_GHC -Wall #-}

module Day09 (solve) where

import           Data.Ord  (comparing, Down (..))
import           Data.List (sortBy)
import           Data.Char (digitToInt)
import           Data.Map  (Map)
import qualified Data.Map                         as M
import qualified Data.Set                         as S

type Coord = (Int,Int)
type Grid  = Map Coord Int

parse :: [[Int]] -> Grid
parse = M.fromList
      . concatMap (\(r, e) -> zipWith (\c d -> ((r,c), d)) [0..] e)
      . zip [0..]

cross :: Grid -> Coord -> Grid
cross g (r, c) = M.restrictKeys g
               $ S.fromList [(r + 1, c), (r - 1, c), (r, c + 1), (r, c - 1)]

lowest :: Grid -> Coord -> Int -> Bool
lowest g (r, c) e = all (>e) . M.elems $ cross g (r, c)

basin :: Grid -> Coord -> Int -> Grid
basin g (r, c) e = M.filter (\e' -> e' > e && e' < 9) $ cross g (r, c)

fillBasin :: Grid -> Grid -> Grid -> Grid
fillBasin g s w | M.null w  = s
                | otherwise = fillBasin g s' w'
  where
    w' = M.foldMapWithKey (basin g) w
    s' = M.union s w

fill :: Grid -> Grid -> [Int]
fill g l = sortBy (comparing Down) bs
  where
    bs = map (M.size . fillBasin g M.empty . uncurry M.singleton) $ M.toList l

solve :: IO ()
solve = do
    input <- parse . map (map digitToInt) . lines <$> readFile path
    let lows   = M.filterWithKey (lowest input) input
        silver = sum . map succ . M.elems $ lows
        gold   = product . take 3 $ fill input lows
    putStrLn $ "\tSilver: " ++ show silver
          ++ "\n\tGold:   " ++ show gold
  where
    path = "./rsc/day09.txt"
