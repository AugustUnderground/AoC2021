module Main where

import Lib

main :: IO ()
main = readFile "./resource/day05.txt" >>= print . day05

-- main :: IO ()
-- main = do
--     readFile input >>= print . day05
--     inp <- readFile input
--     putStrLn "AOC 2021"
--   where 
--     input = "./resource/day05_.txt"
