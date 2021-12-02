module Main where

import Lib

main :: IO ()
main = readFile "./resource/day02.txt" >>= print . day02_2

--main :: IO ()
--main = do
--    readFile input >>= print . day02
--    putStrLn "AOC 2021"
--  where 
--    input = "./resource/day02_.txt"
