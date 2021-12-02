module Lib
    ( intPut
    , day01
    , day01_2
    ) where

import Control.Applicative
import Data.Traversable (sequenceA)
import Data.List (tails)

intPut :: String -> [Int]
intPut = map (\x -> (read x :: Int)) . lines 

day01' :: Int -> [Int] -> Int
day01' a [] = a
day01' a [_] = a
day01' a (z:y:xs) = day01' (if y > z then a + 1 else a) (y:xs)

day01 :: String -> Int
day01 input = day01' 0 (intPut input)

day01_2' :: Int -> [Int] -> [[Int]]
day01_2' m = getZipList . traverse ZipList . take m . tails 

day01_2 :: String -> Int
day01_2 input = day01' 0 (map sum sl)
    where sl = day01_2' 3 (intPut input)
