{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE TypeApplications #-}

module Day21 (solve) where

import qualified Data.Map as M
import           Data.Map      (Map)

tt :: [a] -> (a,a)
tt [x,y] = (x,y)
tt   _   = undefined

parse :: [String] -> (Int, Int)
parse = tt . map (read @Int . drop 1 . dropWhile (/= ':'))

step :: Int -> Int -> Int
step p n = cycle [1 .. 10] !! (p + n - 1)

roll :: Int -> Int
roll n = sum . take 3 . drop n $ cycle [1 .. 100]

game :: Bool -> Int -> Int -> Int -> Int -> Int -> Int
game t p1 p2 s1 s2 nr | s1 >= 1000 = s2 * nr
                      | s2 >= 1000 = s1 * nr
                      | t          = game (not t) p1' p2  s1' s2  nr'
                      | otherwise  = game (not t) p1  p2' s1  s2' nr'
  where
    p1' = step p1 $ roll nr
    p2' = step p2 $ roll nr
    s1' = s1 + p1'
    s2' = s2 + p2'
    nr' = nr + 3

type State = Map (Int, Int) Int

step' :: Int -> [Int]
step' p = map ((cycle [1 .. 10] !!) . pred . (p+)) rolls
  where
    rolls = [ a + b + c | a <- [1..3], b <- [1..3], c <- [1..3] ]

winners :: (Int, Int) -> Int -> Bool
winners (_, s) _ = s >= 21

next' :: (Int, Int) -> Int -> State
next' (p, s) n = M.fromListWith (+) [ ((q, s + q), n) | q <- step' p ] 

next :: (Int, Int) -> Int -> State -> State
next k n m = M.unionWith (+) m $ next' k n

split :: State -> (Int, State)
split s = (sum $ M.elems w, s')
  where
    (w, s') = M.partitionWithKey winners s

game' :: Bool -> State -> State -> (Int, Int)
game' True  s1 s2 | M.null s1'' = (w1 * tot, 0)
                  | otherwise   = (w1' + w1 * tot, w2')
  where
    s1'        = M.foldrWithKey next M.empty s1
    (w1, s1'') = split s1'
    tot        = sum $ M.elems s2
    (w1', w2') = game' False s1'' s2
game' False s1 s2 | M.null s2'' = (0, w2 * tot)
                  | otherwise   = (w1', w2' + w2 * tot)
  where
    s2'        = M.foldrWithKey next M.empty s2
    (w2, s2'') = split s2'
    tot        = sum $ M.elems s1
    (w1', w2') = game' True s1 s2''

solve :: IO ()
solve = do
    (p1,p2) <- parse . lines <$> readFile path
    let s1     = M.singleton (p1, 0) 1
        s2     = M.singleton (p2, 0) 1
        silver = game  True p1 p2 0 0 0
        gold   = uncurry max $ game' True s1 s2
    putStrLn $ "\tSilver: " ++ show silver
          ++ "\n\tGold:   " ++ show gold
  where
    path = "./rsc/day21.txt"
