module Main where

import Lib
import Data.Functor

main :: IO ()
main = do
    readFile input01 >>= print . day01_2

    putStrLn "AOC 2021"
  where 
    input01 = "./resource/day01.txt"
