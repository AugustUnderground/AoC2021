module Main where

import Lib

main :: IO ()
main = readFile "./resource/day04.txt" >>= print . day04

--main :: IO ()
--main = do
--    readFile input >>= print . day04
--    inp <- readFile input
--    putStrLn "AOC 2021"
--  where 
--    input = "./resource/day04_.txt"
