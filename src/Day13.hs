{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

{-# LANGUAGE TypeApplications #-}

module Day13 (solve) where

import           Data.Bifunctor  (first, second)
import           Data.List.Split (splitOn)
import           Data.List       (isPrefixOf)
import           Data.Set        (Set)
import qualified Data.Set                        as S

type Coord = (Int, Int)
data Fold  = X Int | Y Int
  deriving (Eq, Ord, Show)

parseFold :: String -> Fold
parseFold f | "fold along y=" `isPrefixOf` f = Y z
            | "fold along x=" `isPrefixOf` f = X z
            | otherwise                      = undefined
  where
    z = read @Int . drop 1 $ dropWhile (/='=') f

parsePoint :: String -> Coord
parsePoint p = let [a,b] = map (read @Int) $ splitOn "," p in (a,b)

render :: Set Coord -> String
render cs = unlines [ "\t\t" ++ [ if S.member (x,y) cs then '#' else ' '
                    | x <- [0 .. nc] ] | y <- [0 .. nr] ]
  where
    nr  = S.findMax $ S.map snd cs
    nc  = S.findMax $ S.map fst cs

fold :: Set Coord -> Fold -> Set Coord
fold cs (X x) = S.union l r'
  where
    (r, l) = S.partition ((>x) . fst) cs
    r'     = S.map (first ((x-) . subtract x)) r
fold cs (Y y) = S.union a b'
  where
    (b, a) = S.partition ((>y) . snd) cs
    b'     = S.map (second ((y-) . subtract y)) b

solve :: IO ()
solve = do
    [ps',is'] <- splitOn [""] . lines <$> readFile path
    let points = S.fromList $ map parsePoint ps'
        instrs = map parseFold is'
        silver = S.size . fold points $ head instrs
        gold   = ('\n' :) . render $ foldl fold points instrs
    putStrLn $ "\tSilver: " ++ show silver
          ++ "\n\tGold:   " ++ gold
  where
    path = "./rsc/day13.txt"
