{-# OPTIONS_GHC -Wall #-}

module Day15 (solve) where

import           Data.Function (on)
import           Data.List     (minimumBy)
import           Data.Char     (digitToInt)
import           Data.Map      (Map, (!))
import qualified Data.Map                   as M
import           Data.Set      (Set)
import qualified Data.Set                   as S

type Risk  = Int
type Coord = (Int, Int)
type Grid  = Map Coord Risk

both :: (a -> b) -> (a,a) -> (b,b)
both f (x,y) = (f x, f y)

parse :: [String] -> Grid
parse = M.fromList . concat
      . zipWith (\r l -> zipWith (\c i -> ((r,c), digitToInt i)) [0..] l) [0..]

cross' :: Grid -> Coord -> Coord -> Grid
cross' grid (rr,cc) (r,c) = M.fromList
                          [ ((r', c'), ((rs + tr + tc - 1) `mod` 9) + 1)
                          | (r', c') <- [ (r + 1, c), (r - 1, c)
                                        , (r, c + 1), (r, c - 1) ]
                          , let tr = r' `div` (rl+1), let tc = c' `div` (cl+1)
                          , let gr = r' `mod` (rl+1), let gc = c' `mod` (cl+1)
                          , let rs = grid ! (gr, gc)
                          , r' >= 0, c' >= 0, tr <= rr, tc <= cc ]
  where
    (rl, cl) = maximum $ M.keysSet grid

djk :: Grid -> Coord -> Coord -> Set Coord -> Map Coord Risk -> Risk
djk grid expand target visited stack
    | M.null stack          = 0
    | M.member target valid = valid ! target
    | otherwise             = djk grid expand target visited' stack'
  where
    (coord, risk) = minimumBy (compare `on` snd) $ M.toList stack
    visited'      = S.insert coord visited
    valid         = M.map (+ risk) . flip M.withoutKeys visited'
                  $ cross' grid expand coord
    stack'        = M.unionWith min valid $ M.delete coord stack

solve :: IO ()
solve = do
    input <- parse . lines <$> readFile path
    let start'  = minimum $ M.keysSet input
        start   = M.restrictKeys input $ S.singleton start'
        offset  = input ! start'
        target  = maximum $ M.keysSet input
        target' = both (pred . (* 5) . succ) target
        silver  = subtract offset $ djk input (0,0) target  S.empty start
        gold    = subtract offset $ djk input (4,4) target' S.empty start
    putStrLn $ "\tSilver: " ++ show silver
          ++ "\n\tGold:   " ++ show gold
  where
    path = "./rsc/day15.txt"
