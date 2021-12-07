module Main where

import Lib

main :: IO ()
main = readFile "./resource/day07.txt" >>= print . day07

--main :: IO ()
--main = do
--    readFile input >>= print . day05
--    inp <- readFile input
--    putStrLn "AOC 2021"
--  where 
--    input = "./resource/day07.txt"
