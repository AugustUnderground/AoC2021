{-# OPTIONS_GHC -Wall -fno-warn-incomplete-uni-patterns #-}

module Day12 (solve) where

import           Data.List       (nub)
import           Data.Foldable   (fold)
import           Data.Char       (isLower, isUpper)
import           Data.List.Split (splitOn)
import           Data.Map        (Map, (!))
import qualified Data.Map                           as M
import           Data.Set        (Set)
import qualified Data.Set                           as S

type Node  = String
type Graph = Map Node (Set Node)

pairs :: String -> (Node, Node)
pairs l = let [a,b] = splitOn "-" l in (a, b)

build :: [(Node, Node)] -> Graph
build  [] = M.empty
build ((a,b) : nodes) = M.unionWith S.union g $ build nodes
  where
    g = M.fromList [(a, S.singleton b), (b, S.singleton a)]

notTwice :: [Node] -> Bool
notTwice p = let p' = filter (all isLower) p in length p' == length (nub p')

paths :: Graph -> [Node] -> Node -> Set [Node]
paths _ p "end" = S.singleton $ reverse ("end" : p)
paths g p   n   = fold . S.map (paths g (n : p)) . S.filter pr $ g ! n
  where
    pr :: Node -> Bool
    pr e = all isUpper e || notElem e p

paths' :: Graph -> [Node] -> Node -> Set [Node]
paths' _ p "end" = S.singleton $ reverse ("end" : p)
paths' g p   n   = fold . S.map (paths' g p') $ S.filter pr $ g ! n
  where
    p' = n : p
    pr :: Node -> Bool
    pr e = e /= "start" && (all isUpper e || notElem e p || notTwice p')

solve :: IO ()
solve = do
    input <- build . map pairs . lines <$> readFile path
    let silver = S.size $ paths  input [] "start"
        gold   = S.size $ paths' input [] "start"
    putStrLn $ "\tSilver: " ++ show silver
          ++ "\n\tGold:   " ++ show gold
  where
    path = "./rsc/day12.txt"
