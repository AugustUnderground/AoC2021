{-# LANGUAGE TypeApplications #-}

module Day01 (solve) where

depth :: [Int] -> Int
depth []                       = 0
depth [_]                      = 0
depth (a : b : cs) | a < b     = 1 + depth (b : cs)
                   | otherwise = 0 + depth (b : cs)

depth' :: [Int] -> Int
depth' []        = 0
depth' [_]       = 0
depth' [_, _]    = 0
depth' [_, _, _] = 0
depth' (x1 : x2 : x3 : x4 : xs) 
    | (x1 + x2 + x3) < (x2 + x3 + x4) = 1 + depth' (x2 : x3 : x4 : xs)
    | otherwise                       = 0 + depth' (x2 : x3 : x4 : xs)

solve :: IO ()
solve = do
    input <- map (read @Int) . lines <$> readFile path
    let silver = depth input
        gold   = depth' input
    putStrLn $ "\tSilver: " ++ show silver
          ++ "\n\tGold:   " ++ show gold
  where
    path = "./rsc/day01.txt"
