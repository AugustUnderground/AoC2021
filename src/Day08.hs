{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

{-# LANGUAGE TypeApplications #-}

module Day08 (solve) where

import           Data.List.Split (splitOn)
import           Data.List       (elemIndex)
import           Data.Maybe      (fromJust)
import qualified Data.Set                  as S

parse :: String -> ([String], [String])
parse ls = let [i,o] = map words $ splitOn "|" ls in (i,o)

hf  :: (a -> Bool) -> [a] -> a
hf p = head . filter p

deduce :: [String] -> [String] -> Int
deduce inputs = read @Int
              . concatMap ((show . fromJust . (`elemIndex` digs)) . S.fromList)
  where
    ins  = map S.fromList inputs
    [one, seven, four, eight] = map (\n -> hf ((==n) . S.size) ins) [2, 3, 4, 7]
    nine  = hf (\i -> S.size i == 6 && (four `S.isSubsetOf` i)) ins
    zero  = hf (\i -> S.size i == 6 && (seven `S.isSubsetOf` i) && (i /= nine)) ins
    six   = hf (\i -> S.size i == 6 && i /= zero && i /= nine) ins
    five  = hf (\i -> S.size i == 5 && (i `S.isSubsetOf` six)) ins
    three = hf (\i -> S.size i == 5 && (one `S.isSubsetOf` i)) ins
    two   = hf (`notElem` [zero,one,three,four,five,six,seven,eight,nine]) ins
    digs  = [zero,one,two,three,four,five,six,seven,eight,nine]

solve :: IO ()
solve = do
    input <- map parse . lines <$> readFile path
    let silver = length . concatMap (filter ((`elem` [2,3,4,7]) . length) . snd)
               $ input
        gold   = sum $ map (uncurry deduce) input
    putStrLn $ "\tSilver: " ++ show silver
          ++ "\n\tGold:   " ++ show gold
  where
    path = "./rsc/day08.txt"
