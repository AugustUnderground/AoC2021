{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE RecordWildCards #-}

module Day20 (solve) where

import           Control.Monad            (replicateM)
import           Control.Monad.State      (State, modify, evalState, gets)
import qualified Data.Set            as S
import           Data.Set                 (Set)

type Coord = (Int, Int)
type Img   = Set Coord
type Alg   = Set Int

data IS = IS { alg :: Alg, img :: Img, lo :: Coord, hi :: Coord } deriving Show

both :: (a -> b) -> (a,a) -> (b,b)
both f (x,y) = (f x, f y)

b2d :: [Int] -> Int
b2d = foldl (\a x -> 2 * a + x) 0

parse :: [String] -> (Img, Alg)
parse (a' : _ : i') = (i,a)
  where
    a = S.fromList $ [ii | (aa,ii) <- zip a' [0 ..], aa == '#']
    i = S.fromList
      $ concat [ [ (ri,ci) | (col,ci) <- zip row [0 .. ], col == '#' ]
               | (row,ri) <- zip i' [0 .. ] ]
parse _ = undefined

bounds :: Img -> (Coord, Coord)
bounds i = let (r,c) = (S.map fst i, S.map snd i)
            in ( both minimum (r,c), both maximum (r,c) )

window :: Coord -> [Coord]
window (r,c) = [ (r + r', c + c') | r' <- [-1..1], c' <- [-1..1] ]

light :: Alg -> Img -> Coord -> Bool
light a i c = flip S.member a
            $ b2d [ if S.member c' i then 1 else 0 | c' <- window c ]

enhance' :: State IS Img
enhance' = modify (\s@IS{..} -> 
    let (rs,cs) = (S.fromList [fst lo .. fst hi],S.fromList [snd lo .. snd hi])
        img'    = S.filter (light alg img) $ S.cartesianProduct rs cs
     in s { img = img', lo = both (+ 3) lo, hi = both (subtract 3) hi }
  ) >> gets img

enhance :: Int -> IS -> Img
enhance n = last . evalState (replicateM n enhance')

mkImage :: Img -> Alg -> IS
mkImage i a = IS { alg = a, img = i, lo = both (subtract 200) lo'
                 , hi = both (+ 200) hi' }
  where
    (lo', hi') = bounds i

solve :: IO ()
solve = do
    (im, al) <- parse . lines <$> readFile path
    let silver = S.size . enhance  2 $ mkImage im al
        gold   = S.size . enhance 50 $ mkImage im al
    putStrLn $ "\tSilver: " ++ show silver
          ++ "\n\tGold:   " ++ show gold
  where
    path = "./rsc/day20.txt"
