module Main where

import Lib

--main :: IO ()
--main = readFile "./resource/day13.txt" >>= print . day13

main :: IO ()
main = do
    --readFile input >>= print . day05
    inp <- readFile input
    putStrLn "AOC 2021"
  where 
    input = "./resource/day13.txt"
