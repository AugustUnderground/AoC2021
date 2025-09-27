{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TypeApplications #-}

module Day06 (solve) where

import           Data.List.Split      (splitOn)
import           Data.Sequence        (Seq, (|>))
import qualified Data.Sequence   as S

sim :: Int -> Seq Int -> Seq Int
sim 0 fs = fs
sim s fs = sim (pred s) . S.adjust' (+nf) 6 $ S.drop 1 fs |> nf
  where
    nf = S.index fs 0

solve :: IO ()
solve = do
    input <- map (read @Int) . splitOn "," . head . lines <$> readFile path
    let fish   = foldr (S.adjust' (+1)) (S.replicate 9 0) input
        silver = sum $ sim  80 fish
        gold   = sum $ sim 256 fish
    putStrLn $ "\tSilver: " ++ show silver
          ++ "\n\tGold:   " ++ show gold
  where
    path = "./rsc/day06.txt"
