module Main where

import Lib

main :: IO ()
main = readFile "./resource/day10.txt" >>= print . day10

--main :: IO ()
--main = do
--    --readFile input >>= print . day05
--    inp <- readFile input
--    putStrLn "AOC 2021"
--  where 
--    input = "./resource/day10_.txt"
