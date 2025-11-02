{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE RecordWildCards #-}

module Day16 (solve) where

import Data.Char (digitToInt)

data Packet = Literal  { version :: Int, typeID :: Int, value :: Int }
            | Operator { version :: Int, typeID :: Int, lengthID :: Int
                       , payload :: [Packet] }
  deriving (Show, Eq, Ord)

h2b :: Char -> [Int]
h2b c =  let n = digitToInt c in map ((`mod`2) . (n`div`)) [8,4,2,1]

b2d :: [Int] -> Int
b2d = foldl (\a x -> 2 * a + x) 0

b2i :: Bool -> Int
b2i True  = 1
b2i False = 0

parsev :: [Int] -> [Int]
parsev (1 : xs) = take 4 xs ++ parsev (drop 4 xs)
parsev (0 : xs) = take 4 xs
parsev    _     = undefined

parse :: [Int] -> [Packet]
parse (v1:v2:v3:1:0:0:vs)      = packet : parse bs
  where
    val    = parsev vs
    lv     = (length val `div` 4) * 5
    bs     = drop lv vs
    packet = Literal { version = b2d [v1,v2,v3]
                     , typeID  = 4
                     , value   = b2d val }
parse (v1:v2:v3:t1:t2:t3:0:pl) = packet : parse bs
  where
    nb     = b2d $ take 15 pl
    bs     = drop (15 + nb) pl
    packet = Operator { version  = b2d [v1,v2,v3]
                      , typeID   = b2d [t1,t2,t3]
                      , lengthID = 0
                      , payload  = parse . take nb $ drop 15 pl }
parse (v1:v2:v3:t1:t2:t3:1:pl) = (packet :) . drop np $ parse bs
  where
    np     = b2d $ take 11 pl
    bs     = drop 11 pl
    ps     = take np $ parse bs
    packet = Operator { version  = b2d [v1,v2,v3]
                      , typeID   = b2d [t1,t2,t3]
                      , lengthID = 1
                      , payload  = ps }
parse _ = []

ver :: Packet -> Int
ver Literal{..}  = version
ver Operator{..} = (version +) . sum $ map ver payload

dec :: Packet -> Int
dec Literal{..} = value
dec Operator{typeID = 0, .. } = sum     $ map dec payload
dec Operator{typeID = 1, .. } = product $ map dec payload
dec Operator{typeID = 2, .. } = minimum $ map dec payload
dec Operator{typeID = 3, .. } = maximum $ map dec payload
dec Operator{typeID = 5, payload = p1 : p2 : _ } = b2i $ dec p1  > dec p2
dec Operator{typeID = 6, payload = p1 : p2 : _ } = b2i $ dec p1  < dec p2
dec Operator{typeID = 7, payload = p1 : p2 : _ } = b2i $ dec p1 == dec p2
dec _ = 0

solve :: IO ()
solve = do
    input <- head . lines <$> readFile path
    let packet  = head . parse $ concatMap h2b input
        silver  = ver packet
        gold    = dec packet
    putStrLn $ "\tSilver: " ++ show silver
          ++ "\n\tGold:   " ++ show gold
  where
    path = "./rsc/day16.txt"
