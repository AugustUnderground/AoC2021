module Main where

import Lib
import Data.Functor

--main :: IO ()
--main = readFile "./resource/day15.txt" >>= print . day15

main :: IO ()
main = do
    --readFile input >>= print . day05
    inp <- readFile input <&> (head . lines)
    putStrLn "AOC 2021"
  where 
    input = "./resource/day16_.txt"
