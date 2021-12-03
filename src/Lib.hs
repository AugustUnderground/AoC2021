module Lib
    ( intPut
    , mapPut
    , day01
    , day01_2
    , day02
    , day02_2
    , day03
    , day03_2
    ) where

import Control.Applicative
import Data.List
import Data.Char (digitToInt, intToDigit)

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

toDec :: String -> Int
toDec = foldl' (\a x -> a * 2 + digitToInt x) 0

day03' :: [Int] -> Int
day03' [zero,one] | zero >  one = 0
                  | zero <  one = 1
                  | zero == one = 1
                  | otherwise  = error "You screwed up day 3!"
day03' _ = error "You screwed up day 3!!"

day03'' :: [Int] -> Int
day03'' [zero,one] | zero < one = 0
                   | zero > one = 1
                   | otherwise  = error "You screwed up day 3!"
day03'' _ = error "You screwed up day 3!"

day03''' :: ([Int] -> Int) -> String -> Int
day03''' fn = toDec . concatMap (show . fn . (map length . group . sort)) 
            . transpose . lines

gammaRate :: String -> Int
gammaRate = day03''' day03'

epsilonRate :: String -> Int
epsilonRate = day03''' day03''

day03 :: String -> Int
day03 inp = gamma * epsilon
  where gamma   = gammaRate inp
        epsilon = epsilonRate inp

length' :: [String] -> [Int]
length' [zs,os] = [length zs, length os]
length' [zos]   | nub zos == "1" = [0, length zos]
                | nub zos == "0" = [length zos, 0]
                | otherwise = error "You screwed up Day 3!"
length' _ = error "You screwed up Day 3!!!!!"

findMCB :: [String] -> Int
findMCB = day03' . head . map (length' . group . sort) . transpose

day03_2' :: Int -> [String] -> String
day03_2' idx lb | length lb > 1 = let mcb = intToDigit . findMCB . map (drop idx) $ lb
                                   in day03_2' (idx + 1) (filter (\b -> (b !! idx) == mcb) lb)
                | otherwise = head lb

day03_2'' :: Int -> [String] -> String
day03_2'' idx lb | length lb > 1 = let mcb = intToDigit . findMCB . map (drop idx) $ lb
                                    in day03_2'' (idx + 1) (filter (\b -> (b !! idx) /= mcb) lb)
                 | otherwise = head lb

day03_2 :: String -> Int
day03_2 inp = ogr * csr
  where ogr = toDec . day03_2'  0 . lines $ inp
        csr = toDec . day03_2'' 0 . lines $ inp

