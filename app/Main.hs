module Main where

import Lib
import Data.Functor

main :: IO ()
main = readFile "./resource/day18.txt" >>= print . day18

-- main :: IO ()
-- main = do
--     --readFile input >>= print . day05
--     inp <- readFile input <&> (head . lines)
--     inp <- readFile input <&> lines
--     inp <- readFile input 
--     putStrLn "AOC 2021"
--   where 
--     input = "./resource/day18_.txt"
