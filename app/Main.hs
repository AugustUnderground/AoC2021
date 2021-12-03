module Main where

import Lib

main :: IO ()
main = readFile "./resource/day03.txt" >>= print . day03_2

--main :: IO ()
--main = do
--    readFile input >>= print . day03
--    inp <- readFile input
--    putStrLn "AOC 2021"
--  where 
--    input = "./resource/day03_.txt"
