{-# OPTIONS_GHC -Wall -fno-warn-incomplete-uni-patterns #-}

{-# LANGUAGE TypeApplications #-}

module Day17 (solve) where

import           Text.Regex.TDFA ((=~))
import           Data.Set        (Set)
import qualified Data.Set               as S

type Coord = (Int, Int)

fth :: (a,b,c,d) -> d
fth (_,_,_,x) = x

parse :: String -> Set Coord
parse i = S.fromList [ (x,y) | x <- [x0 .. x1], y <- [y0 .. y1]]
  where
    p = "target area: x=(-?[0-9]+)..(-?[0-9]+), y=(-?[0-9]+)..(-?[0-9]+)"
    [x0,x1,y0,y1] = map (read @Int)
                  $ fth (i =~ p :: (String, String, String, [String]))

dom :: Coord -> Set Coord -> Bool
dom (x,y) = S.foldr ir False
  where
    ir (x',y') = (|| ((x' >= x) && (y' <= y)))

step :: Set Coord -> Coord -> Coord -> [Coord]
step tgt (vx, vy) (x, y) | dom (x,y) tgt = (x,y) : step tgt (vx', vy') (x', y')
                         | otherwise     = []
  where
    x'  = x + vx
    y'  = y + vy
    vx' = max 0 $ vx - 1
    vy' = vy - 1

trajectory :: Set Coord -> Coord -> Set Coord
trajectory tgt vel | S.null $ S.intersection trj tgt = S.empty
                   | otherwise                       = trj
  where
    trj = S.fromList $ step tgt vel (0,0)

hit :: Set Coord -> Set Coord -> Bool
hit tgt = not . S.null . S.intersection tgt

solve' :: Set Coord -> (Int, Int)
solve' tgt = (maximum trjs, length trjs)
  where
    y0   = S.findMin $ S.map snd tgt
    y1   = abs y0
    trjs = [ hgt | vx <- [1 .. 1000], vy <- [y0 .. y1]
                 , let trj = trajectory tgt (vx, vy)
                 , let hgt = S.findMax $ S.map snd trj
                 , hit tgt trj ]

solve :: IO ()
solve = do
    input <- parse . head . lines <$> readFile path
    let (silver, gold) = solve' input
    putStrLn $ "\tSilver: " ++ show silver
          ++ "\n\tGold:   " ++ show gold
  where
    path = "./rsc/day17.txt"
