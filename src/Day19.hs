{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE TypeApplications #-}

module Day19 (solve) where

import           Data.Set             (Set)
import qualified Data.Set        as S
import           Data.List.Split      (splitOn)
import           Data.Maybe           (listToMaybe)

type Coord     = (Int, Int, Int)
type Transform = Coord -> Coord

parse' :: [Int] -> Coord
parse' [x,y,z] = (x,y,z)
parse'    _    = undefined

parse :: [String] -> [Set Coord]
parse [] = []
parse ls = cs : parse ls'
  where
    cs  = S.fromList . map (parse' . map (read @Int) . splitOn ",")
        . takeWhile (not . null) $ drop 1 ls
    ls' = drop 1 $ dropWhile (not . null) ls

(<->) :: Coord -> Coord -> Coord
(<->) (x,y,z) (x',y',z') = (x - x', y - y', z - z')

(<+>) :: Coord -> Coord -> Coord
(<+>) (x,y,z) (x',y',z') = (x + x', y + y', z + z')

(|-) :: Coord -> Coord -> Bool
(|-) (x1,y1,z1) (x2,y2,z2) = x1*x2 + y1*y2 + z1*z2 == 0

det :: Coord -> Coord -> Coord -> Int
det (a,b,c) (d,e,f) (g,h,i) = a*(e*i - f*h) - b*(d*i - f*g) + c*(d*h - e*g)

(-|) :: Coord -> Coord -> Int
(-|) (x, y, z) (x',y',z') = abs (x - x') + abs (y - y') + abs (z - z')

rot :: [Transform]
rot = [ \(x,y,z) -> (a*x + b*y + c*z, d*x + e*y + f*z, g*x + h*y + i*z)
      |  ( (a,b,c), (d,e,f), (g,h,i) ) <- r ]
  where
    r = [ (r1, r2, r3) | r1 <- q, r2 <- q,  r1 |- r2
                       , r3 <- q, r1 |- r3, r2 |- r3
                       , det r1 r2 r3 == 1 ]
    q = [(1,0,0), (-1,0,0), (0,1,0), (0,-1,0), (0,0,1), (0,0,-1) ]

olp :: Int -> Set Coord -> Set Coord -> Maybe (Transform, Coord, Set Coord)
olp n c c' = listToMaybe [ (r, s, t)
                         | r <- rot, p <- S.toList c , q <- S.toList c'
                         , let s = p <-> r q
                         , let t = S.map ((<+> s) . r) c'
                         , (>=n) . S.size $ S.intersection c t ]

merge' :: Int -> Set Coord -> Set Coord -> Maybe (Transform, Coord, Set Coord)
merge' n c c' = do
  (r, s, t) <- olp n c c'
  pure (r, s, S.union c t)

merge :: Int -> Set Coord -> Set Coord -> [Set Coord] -> (Int, Int)
merge _ d g      []  = (b, m)
  where
    b = S.size g
    m = S.foldl (\a p -> max a $ uncurry (-|) p) 0 . S.filter (uncurry (/=))
      $ S.cartesianProduct d d
merge n d g (c : cs) = case merge' n g c of
  Just (_, t, g') -> let d' = S.insert t d in merge n d' g' cs
  Nothing         -> let cs' = cs ++ [c]   in merge n d  g  cs'

solve :: IO ()
solve = do
    input <- parse . lines <$> readFile path
    let origin         = S.singleton (0,0,0)
        (silver, gold) = merge 12 origin (head input) $ tail input
    putStrLn $ "\tSilver: " ++ show silver
          ++ "\n\tGold:   " ++ show gold
  where
    path = "./rsc/day19.txt"
