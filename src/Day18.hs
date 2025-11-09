{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE InstanceSigs #-}

module Day18 (solve) where

import Data.Char (digitToInt)

data SFN = SFL Int
         | SFP SFN SFN
  deriving (Eq, Ord)

instance Show SFN where
  show :: SFN -> String
  show (SFL n)   = show n
  show (SFP x y) = "[" ++ show x ++ "," ++ show y ++ "]"

data SFE = SFE SFN Int Int Bool
  deriving (Eq, Ord, Show)

data SFS = SFS SFN Bool
  deriving (Eq, Ord, Show)

parse' :: String -> (SFN, String)
parse' ('[' : sn) = (SFP x y, tail yr)
  where
    (x, xr) = parse' sn
    (y, yr) = parse' $ tail xr
parse' ( n  : sn) = (SFL $ digitToInt n, sn)
parse' ""   = undefined

parse :: String -> SFN
parse = fst . parse'

(~+~) :: SFN -> SFN -> SFN
(~+~) = SFP

(~+) :: Int -> SFN -> SFN
(~+) 0      n    = n
(~+) x (SFL n)   = SFL $ x + n
(~+) x (SFP l r) = SFP (x ~+ l) r

(+~) :: SFN -> Int -> SFN
(+~) n         0 = n
(+~) (SFL n)   x = SFL $ x + n
(+~) (SFP l r) x = SFP l (r +~ x)

(~@~) :: Int -> SFN -> SFE
(~@~) _ n@(SFL _)                           = SFE      n             0  0 False
(~@~) d p@(SFP (SFL x) (SFL y)) | d >= 4    = SFE (SFL 0)            x  y True
                                | otherwise = SFE      p             0  0 False
(~@~) d p@(SFP l r) | le                    = SFE (SFP ln (ly ~+ r)) lx 0 True
                    | re                    = SFE (SFP (l +~ rx) rn) 0  ry True
                    | otherwise             = SFE      p             0  0 False
  where
    (SFE ln lx ly le) = (d + 1) ~@~ l
    (SFE rn rx ry re) = (d + 1) ~@~ r

spl :: SFN -> SFS
spl (SFP x y) | xs        = SFS (SFP x' y)  True
              | ys        = SFS (SFP x  y') True
              | otherwise = SFS (SFP x  y)  False
  where
    (SFS x' xs) = spl x
    (SFS y' ys) = spl y
spl (SFL x) | x > 9       = SFS (SFP x' y') True
            | otherwise   = SFS (SFL x)     False
  where
    x' = SFL $ x `div` 2
    y' = SFL $ (x + 2 - 1) `div` 2

sfr :: SFN -> SFN
sfr n | e         = sfr n'
      | s         = sfr n''
      | otherwise =     n''
  where
    (SFE n'  _ _ e) = 0 ~@~ n
    (SFS n''     s) = spl n'

mag :: SFN -> Int
mag (SFP (SFL x) (SFL y)) = 3 * x     + 2 * y
mag (SFP (SFL x)      p)  = 3 * x     + 2 * mag p
mag (SFP      p  (SFL y)) = 3 * mag p + 2 * y
mag (SFP      l       r)  = 3 * mag l + 2 * mag r
mag           _           = undefined

solve :: IO ()
solve = do
    input <- map parse . lines <$> readFile path
    let silver = mag $ foldl1 (\l r -> sfr (l ~+~ r)) input
        gold   = maximum [ mag . sfr $ x ~+~ y
                         | x <- input, y <- input, x /= y ]
    putStrLn $ "\tSilver: " ++ show silver
          ++ "\n\tGold:   " ++ show gold
  where
    path = "./rsc/day18.txt"
