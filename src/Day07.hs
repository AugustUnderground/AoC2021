{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TypeApplications #-}

module Day07 (solve) where

import Data.List.Split (splitOn)

cost :: [Int] -> Int
cost crabs = minimum [ sum . map (abs . (r-)) $ crabs 
                     | r <- [minimum crabs .. maximum crabs] ]

cost' :: [Int] -> Int
cost' crabs = minimum [ sum . map (fuel r) $ crabs 
                      | r <- [minimum crabs .. maximum crabs] ]
  where
    fuel a b = sum [0 .. abs $ a - b]

solve :: IO ()
solve = do
    input <- map (read @Int) . splitOn "," . head . lines <$> readFile path
    let silver = cost  input
        gold   = cost' input
    putStrLn $ "\tSilver: " ++ show silver
          ++ "\n\tGold:   " ++ show gold
  where
    path = "./rsc/day07.txt"
