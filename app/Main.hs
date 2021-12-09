module Main where

import Lib

main :: IO ()
main = readFile "./resource/day09.txt" >>= print . day09

--main :: IO ()
--main = do
--    --readFile input >>= print . day05
--    inp <- readFile input
--    putStrLn "AOC 2021"
--  where 
--    input = "./resource/day09_.txt"
