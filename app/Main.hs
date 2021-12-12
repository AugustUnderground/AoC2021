module Main where

import Lib

main :: IO ()
main = readFile "./resource/day12.txt" >>= print . day12

--main :: IO ()
--main = do
--    --readFile input >>= print . day05
--    inp <- readFile input
--    putStrLn "AOC 2021"
--  where 
--    input = "./resource/day12_.txt"
