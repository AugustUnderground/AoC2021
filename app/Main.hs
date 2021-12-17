module Main where

import Lib
import Data.Functor

--main :: IO ()
--main = readFile "./resource/day16.txt" >>= print . day16

main :: IO ()
main = do
    --readFile input >>= print . day05
    inp <- readFile input <&> (head . lines)
    inp <- readFile input <&> lines
    inp <- readFile input 
    putStrLn "AOC 2021"
  where 
    input = "./resource/day17.txt"
