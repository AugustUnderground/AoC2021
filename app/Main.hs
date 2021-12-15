module Main where

import Lib

main :: IO ()
main = readFile "./resource/day14.txt" >>= print . day14

--main :: IO ()
--main = do
--    --readFile input >>= print . day05
--    inp <- readFile input
--    putStrLn "AOC 2021"
--  where 
--    input = "./resource/day14_.txt"
