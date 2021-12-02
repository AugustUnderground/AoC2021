module Lib
    ( intPut
    , mapPut
    , day01
    , day01_2
    , day02
    , day02_2
    ) where

import Control.Applicative
import Data.List (tails)

intPut :: String -> [Int]
intPut = map (\x -> (read x :: Int)) . lines 

mapPut' :: [String] -> (String, Int)
mapPut' [k, v] = (k, read v :: Int)
mapPut' _ = error "You screwed up AoC 2021 Day 2!"

mapPut :: String -> [(String, Int)]
mapPut = map (mapPut' . words) . lines 

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

day02' :: (String, Int) -> (Int, Int) -> (Int, Int)
day02' ("forward", x) (x', y') = (x' + x, y'    )
day02' ("down",    y) (x', y') = (x'    , y' + y)
day02' ("up",      y) (x', y') = (x'    , y' - y)
day02' (_,         _) (_ ,_  ) = error "You screwed up AoC 2021 Day 2!"

day02 :: String -> Int
day02 = uncurry (*) . foldr day02' (0,0) . mapPut

day02_2' :: (Int, Int, Int) -> (String, Int) -> (Int, Int, Int)
day02_2' (x', y', a') ("forward", x) = (x' + x, y' + (a' * x) , a'    )
day02_2' (x', y', a') ("down",    y) = (x'    , y'            , a' + y)
day02_2' (x', y', a') ("up",      y) = (x'    , y'            , a' - y)
day02_2' (_ ,_  , _ ) (_, _        ) = error "You screwed up AoC 2021 Day 2!"

day02_2 :: String -> Int
day02_2 = pos . foldl day02_2' (0,0,0) . mapPut
  where pos (x, y, a) = x * y
