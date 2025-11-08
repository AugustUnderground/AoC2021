{-# OPTIONS_GHC -Wall -fno-warn-incomplete-uni-patterns #-}

{-# LANGUAGE TupleSections #-}

module Day14 (solve) where

import           Data.List.Split (splitOn)
import           Data.Set        (Set)
import qualified Data.Set                   as S
import           Data.Map        (Map, (!))
import qualified Data.Map                   as M

rules' :: String -> (String, [String])
rules' l = let [[a,b],c] = splitOn " -> " l in ([a,b],[a:c, c ++ [b]])

pairs' :: String -> Map String Int
pairs' (a : b : cs) = M.insertWith (+) [a, b] 1 . pairs' $ b :cs
pairs'        _     = M.empty

occ' :: (String, Int) -> Map Char Int
occ' ([a,b], c) = M.fromListWith (+) [(a,c), (b,c)]
occ'    _       = M.empty

step :: Int -> Map String [String] -> Map String Int -> Map String Int
step 0 _ p = p
step i r p = step (pred i) r . M.fromListWith (+) . concatMap lu $ M.toList p
  where
    lu (q,c) = map (,c) $ r ! q

find :: Set Char -> Map String Int -> Int
find c p = maximum (M.elems occ) - minimum (M.elems occ)
  where
    f k v | S.member k c = ((v - 1) `div` 2) + 1
          | otherwise    = v `div` 2
    occ = M.mapWithKey f . M.unionsWith (+) . map occ' $ M.toList p

solve :: IO ()
solve = do
    input <- lines <$> readFile path
    let temp   = head input
        corner = S.fromList [head temp, last temp]
        pairs  = pairs' temp
        rules  = M.fromList . map rules' $ drop 2 input
        silver = find corner $ step 10 rules pairs
        gold   = find corner $ step 40 rules pairs
    putStrLn $ "\tSilver: " ++ show silver
          ++ "\n\tGold:   " ++ show gold
  where
    path = "./rsc/day14.txt"
