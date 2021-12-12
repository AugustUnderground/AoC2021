module Lib
    ( intPut
    , mapPut
    , day01
    , day01_2
    , day02
    , day02_2
    , day03
    , day03_2
    , day04
    , day05
    , day06
    , day07
    , day08
    , day09
    , day11
    , day12
    ) where

import Control.Applicative
import Debug.Trace
import Data.List
import Data.List.Split
import Data.Char (digitToInt, intToDigit, isUpper, isLower)
import Data.Maybe (mapMaybe, catMaybes, isNothing, isJust, fromJust)
import Data.Tuple (swap)
import qualified Data.Map as Map
import qualified Data.Set as Set

intPut :: String -> [Int]
intPut = map (\x -> (read x :: Int)) . lines 

mapPut' :: [String] -> (String, Int)
mapPut' [k, v] = (k, read v :: Int)
mapPut' _ = error "You screwed up AoC 2021 Day 2!"

mapPut :: String -> [(String, Int)]
mapPut = map (mapPut' . words) . lines 

day01' :: Int -> [Int] -> Int
day01' a [] = a
day01' a [_] = a
day01' a (z:y:xs) = day01' (if y > z then a + 1 else a) (y:xs)

day01 :: String -> Int
day01 input = day01' 0 (intPut input)

day01_2' :: Int -> [Int] -> [[Int]]
day01_2' m = getZipList . traverse ZipList . take m . tails 

day01_2 :: String -> Int
day01_2 input = day01' 0 (map sum sl)
  where sl = day01_2' 3 (intPut input)

day02' :: (String, Int) -> (Int, Int) -> (Int, Int)
day02' ("forward", x) (x', y') = (x' + x, y'    )
day02' ("down",    y) (x', y') = (x'    , y' + y)
day02' ("up",      y) (x', y') = (x'    , y' - y)
day02' (_,         _) (_ ,_  ) = error "You screwed up AoC 2021 Day 2!"

day02 :: String -> Int
day02 = uncurry (*) . foldr day02' (0,0) . mapPut

day02_2' :: (Int, Int, Int) -> (String, Int) -> (Int, Int, Int)
day02_2' (x', y', a') ("forward", x) = (x' + x, y' + (a' * x) , a'    )
day02_2' (x', y', a') ("down",    y) = (x'    , y'            , a' + y)
day02_2' (x', y', a') ("up",      y) = (x'    , y'            , a' - y)
day02_2' (_ ,_  , _ ) (_, _        ) = error "You screwed up AoC 2021 Day 2!"

day02_2 :: String -> Int
day02_2 = pos . foldl day02_2' (0,0,0) . mapPut
  where pos (x, y, a) = x * y

toDec :: String -> Int
toDec = foldl' (\a x -> a * 2 + digitToInt x) 0

day03' :: [Int] -> Int
day03' [zero,one] | zero >  one = 0
                  | zero <  one = 1
                  | zero == one = 1
                  | otherwise  = error "You screwed up day 3!"
day03' _ = error "You screwed up day 3!!"

day03'' :: [Int] -> Int
day03'' [zero,one] | zero < one = 0
                   | zero > one = 1
                   | otherwise  = error "You screwed up day 3!"
day03'' _ = error "You screwed up day 3!"

day03''' :: ([Int] -> Int) -> String -> Int
day03''' fn = toDec . concatMap (show . fn . (map length . group . sort)) 
            . transpose . lines

gammaRate :: String -> Int
gammaRate = day03''' day03'

epsilonRate :: String -> Int
epsilonRate = day03''' day03''

day03 :: String -> Int
day03 inp = gamma * epsilon
  where gamma   = gammaRate inp
        epsilon = epsilonRate inp

length' :: [String] -> [Int]
length' [zs,os] = [length zs, length os]
length' [zos]   | nub zos == "1" = [0, length zos]
                | nub zos == "0" = [length zos, 0]
                | otherwise = error "You screwed up Day 3!"
length' _ = error "You screwed up Day 3!!!!!"

findMCB :: [String] -> Int
findMCB = day03' . head . map (length' . group . sort) . transpose

day03_2' :: Int -> [String] -> String
day03_2' idx lb | length lb > 1 = let mcb = intToDigit . findMCB . map (drop idx) $ lb
                                   in day03_2' (idx + 1) (filter (\b -> (b !! idx) == mcb) lb)
                | otherwise = head lb

day03_2'' :: Int -> [String] -> String
day03_2'' idx lb | length lb > 1 = let mcb = intToDigit . findMCB . map (drop idx) $ lb
                                    in day03_2'' (idx + 1) (filter (\b -> (b !! idx) /= mcb) lb)
                 | otherwise = head lb

day03_2 :: String -> Int
day03_2 inp = ogr * csr
  where ogr = toDec . day03_2'  0 . lines $ inp
        csr = toDec . day03_2'' 0 . lines $ inp

bingo' :: [Int] -> [[Int]] -> Int
bingo' draw board = u * d
  where d' = Set.fromList draw
        b' = Set.fromList . concat $ board
        u  = sum . Set.toList $ Set.difference b' d'
        d  = last draw

bingo :: [Int] -> [[Int]] -> Maybe Int
bingo []   _     = Nothing
bingo draw board = if w || w' 
                      then Just (bingo' draw board)
                      else Nothing
  where b  = map Set.fromList board
        b' = map Set.fromList . transpose $ board
        d  = Set.fromList draw
        w  = any (`Set.isSubsetOf` d) b
        w' = any (`Set.isSubsetOf` d) b'

day04 :: String -> Int
day04 inp = win'
  where draws   = dropWhile null . inits . map read . splitOn "," 
                . head . lines $ inp :: [[Int]]
        boards  = chunksOf 5 . map (map read . words) 
                . filter (/= "") . tail . lines $ inp :: [[[Int]]]
        win = fromJust . head . dropWhile isNothing 
            . concatMap (\d -> map (bingo d) boards) $ draws
        lastWinIdx = head . head . dropWhile (\x -> length x > 1) 
                   . map (elemIndices Nothing) . dropWhile (all isNothing) 
                   . map (\d -> map (bingo d) boards) $ draws
        win' = fromJust . (!!lastWinIdx) . head . dropWhile (any isNothing) 
             . map (\d -> map (bingo d) boards) $ draws

idxList :: [[Int]] -> [(Int, Int)]
idxList [[x1,y1], [x2,y2]] = [ (x, y) 
                             | let (x',x'') = if x1 < x2 then (x1,x2) else (x2,x1)
                             , let (y',y'') = if y1 < y2 then (y1,y2) else (y2,y1)
                             , x <- [x' .. x'']
                             , y <- [y' .. y'']]
idxList _ = error "You Screwed up Day 5!"

idxList' :: [[Int]] -> [(Int, Int)]
idxList' [[x1,y1], [x2,y2]] = zip xs ys
  where xs = if x1 < x2 then [x1 .. x2]
                        else [x1, (x1 - 1) .. x2]
        ys = if y1 < y2 then [y1 .. y2]
                        else [y1, (y1 - 1) .. y2]
idxList' _ = error "You Screwed up Day 5!"

day05 :: String -> Int
day05 inp = overlap'
  where 
    coords = map (map (map read . splitOn ",") . splitOn "->") . lines 
           $ inp :: [[[Int]]]
    grid = filter (\[a, b] -> a!!0 == b!!0 || a!!1 == b!!1) coords
    points = concatMap idxList grid
    overLap' (x1, y1) (x2, y2) = x1 == x2 && y1 == y2
    overlap = length . filter (>1) . map length . groupBy overLap' . sort $ points
    diag = filter (\[a, b] -> a!!0 /= b!!0 && a!!1 /= b!!1) coords
    points' = concatMap idxList' diag
    overlap' = length . filter (>1) . map length . groupBy overLap' . sort $ points ++ points'

population :: Int -> [Int] -> [Int]
population 0   states = states
population day states = population (day - 1) newStates
  where 
    resetStates = head states
    shiftStates = tail states ++ [0]
    newStates   = [ if (s == 8) || (s == 6) 
                       then f + resetStates 
                       else f 
                  | (s, f) <- zip [0 .. 8] shiftStates ]

day06 :: String -> Int
day06 inp = sum fish'
  where
    days = 256
    fish = map read . splitOn "," . head . lines $ inp :: [Int]
    countStates s = length . filter (s ==)
    states = map (`countStates` fish) [0 .. 8]
    fish' = population days states

fuel :: Int -> Int -> Int
fuel a b = abs $ a - b

fuel' :: Int -> Int -> Int
fuel' a b = sum [0 .. abs $ a - b]

day07 :: String -> Int
day07 inp = minimum cost
  where
    crabs = map read . splitOn "," . head . lines $ inp :: [Int]
    min'  = minimum crabs
    max'  = maximum crabs
    range = [min' .. max']
    cost  = [ sum . map (fuel' r) $ crabs | r <- range ]
    --pos   = fromJust $ elemIndex (minimum cost) cost

toTuple :: [a] -> (a,a)
toTuple [x,y] = (x,y)
toTuple _ = error "Screwed up day 8!"

decode :: ([String], [String]) -> Int
decode (input,output) = read . concatMap (show . fromJust . (`elemIndex` ds)) $ os
  where
    is    = map (Set.fromList . sort) input
    os    = map (Set.fromList . sort) output
    one   = head . filter ((==2) . Set.size) $ is
    four  = head . filter ((==4) . Set.size) $ is
    seven = head . filter ((==3) . Set.size) $ is
    eight = head . filter ((==7) . Set.size) $ is
    nine  = head . filter (\n -> Set.size n == 6 && (four `Set.isSubsetOf` n)) $ is
    zero  = head . filter (\z -> Set.size z == 6 && (seven `Set.isSubsetOf` z) && (z /= nine)) $ is
    six   = head . filter (\s -> Set.size s == 6 && notElem s [zero, nine]) $ is
    five  = head . filter (\t -> Set.size t == 5 && (t `Set.isSubsetOf` six)) $ is
    three = head . filter (\t -> Set.size t == 5 && (one `Set.isSubsetOf` t)) $ is
    ds'   = [one,three,four,five,six,seven,eight,nine,zero]
    two   = head . filter (`notElem` ds') $ is
    ds    = [zero,one,two,three,four,five,six,seven,eight,nine]

day08 :: String -> Int
day08 = sum . map (decode . toTuple . map words . splitOn "|") . lines
    --uniqueLengths = [2,3,4,7]
    --uniqueDigits  = sum . map (length . filter (`elem` uniqueLengths) 
    --                                  . map length . words . (!!1) . splitOn "|") 
    --              . lines $ inp

adjacentPoints :: [[Int]] -> Int -> Int -> [(Int, Int)]
adjacentPoints grid row col 
    | (row + 1) == nRows && (col + 1) == nCols = [(row - 1, col), (row, col - 1)]
    | row == 0           && col == 0           = [(row + 1, col), (row, col + 1)]
    | row == 0           && (col + 1) == nCols = [(row + 1, col), (row, col - 1)]
    | (row + 1) == nRows && col == 0           = [(row - 1, col), (row, col + 1)]
    | row == 0           && col > 0 && (col + 1) < nCols = [(row, col - 1), (row, col + 1), (row + 1, col)]
    | (row + 1 == nRows) && col > 0 && (col + 1) < nCols = [(row, col - 1), (row, col + 1), (row - 1, col)]
    | col == 0           && row > 0 && (row + 1) < nRows = [(row - 1, col), (row + 1, col), (row, col + 1)]
    | (col + 1 == nCols) && row > 0 && (row + 1) < nRows = [(row - 1, col), (row + 1, col), (row, col - 1)]
    | otherwise =[(row + 1, col), (row - 1, col), (row, col + 1), (row, col - 1)]
  where
    nRows = length grid
    nCols = length . head $ grid

localMinimum :: [[Int]] -> (Int, Int) -> [(Int, Int)] -> Maybe Int
localMinimum grid (row, col) neighbours = if all (> value) adj
                                               then Just value
                                               else Nothing
  where
    get r c = grid !! r !! c
    value   = get row col
    adj     = map (uncurry get) neighbours

localMinimum' :: [[Int]] -> (Int, Int) -> [(Int, Int)] -> (Maybe Int, (Int, Int))
localMinimum' grid (row, col) neighbours = if isNothing lm
                                              then (Nothing, (-1,-1))
                                              else (lm, (row, col))
    where lm = localMinimum grid (row, col) neighbours

basinPoints :: [[Int]] -> Int -> [(Int, Int)] -> [(Int, Int)] -> Int
basinPoints _    bs  _         []           = bs
basinPoints grid bs rcs' ((row, col) : rcs) = basinPoints grid (bs + 1)
                                                          (nub ((row,col) : rcs'))
                                                          (nub (rcs ++ adj))
  where
    get r c = grid !! r !! c
    adj     = filter (\(r,c) -> get r c < 9 && notElem (r,c) rcs') $ adjacentPoints grid row col

day09 :: String -> Int
day09 inp = mins'
  where
    grid = map (map digitToInt) . lines $ inp
    nRows = length grid
    nCols = length . head $ grid
    --mins = catMaybes [ localMinimum grid (r,c) (adjacentPoints grid r c) 
    --                 | r <- [0 .. nRows - 1] , c <- [0 .. nCols - 1] ]
    --mins' = (+ length mins) . sum $ mins
    mins = map snd . filter (\(m, _ ) -> isJust m) 
         $ [ localMinimum' grid (r,c) (adjacentPoints grid r c) 
           | r <- [0 .. nRows - 1] , c <- [0 .. nCols - 1] ]
    basins = map (\m -> basinPoints grid 0 [] [m]) mins
    mins' = product . take 3 . reverse . sort $ basins

adjacentOctos :: (Int, Int) -> [(Int, Int)]
adjacentOctos (r,c) = [(r',c') | r' <- [ r - 1 .. r + 1 ], c' <- [ c - 1 .. c + 1]]

count :: Eq a => a -> [a] -> Int
count e l = length $ elemIndices e l

octoCharge :: [(Int, Int)] -> [[Int]] -> (Int, Int) -> Int
octoCharge coords grid (r,c) | g >= 10   = -(g + 1)
                             | g < 0     = 0
                             | otherwise = count (r,c) coords
  where
    g = grid !! r !! c

octoStep :: Int -> Int -> [[Int]] -> [(Int, Int)] -> Int
octoStep 0 flashes grid coords = flashes
octoStep steps flashes grid [] = octoStep (steps - 1) flashes grid' coords'
  where
    nRows   = length grid
    nCols   = length . head $ grid
    coords' = [(r,c) | r <- [0 .. nRows - 1], c <- [0 .. nCols - 1]]
    grid'   = map (map (\g -> if g >= 10 || g < 0 then 0 else g)) grid
octoStep steps flashes grid coords = octoStep steps flashes' grid' coords'
  where
    nRows       = length grid
    nCols       = length . head $ grid
    wholeGrid   = [(r,c) | r <- [0 .. nRows - 1], c <- [0 .. nCols - 1]]
    increment   = chunksOf nRows . map (octoCharge coords grid) $ wholeGrid
    grid'       = zipWith (zipWith (+)) grid increment
    flashCoords = concatMap (\(r,c) -> zip [r,r..] c) . filter (not . null . snd)
                $ zip [ 0 .. length grid' ] (map (findIndices (>9)) grid')
    flashes'    = flashes + length flashCoords
    coords'     = filter (`notElem` flashCoords)
                . filter (\(r,c) -> r >= 0 && r < nRows && c >= 0 && c < nCols)
                . concatMap adjacentOctos $ flashCoords

octoStep' :: Int -> Int -> [[Int]] -> [(Int, Int)] -> Int
octoStep' steps flashes grid [] = octoStep' (steps + 1) flashes grid' coords'
  where
    nRows   = length grid
    nCols   = length . head $ grid
    coords' = [(r,c) | r <- [0 .. nRows - 1], c <- [0 .. nCols - 1]]
    grid'   = map (map (\g -> if g >= 10 || g < 0 then 0 else g)) grid
octoStep' steps flashes grid coords | all (all (==0)) grid = steps
                                    | otherwise = octoStep' steps flashes' grid' coords'
  where
    nRows       = length grid
    nCols       = length . head $ grid
    wholeGrid   = [(r,c) | r <- [0 .. nRows - 1], c <- [0 .. nCols - 1]]
    increment   = chunksOf nRows . map (octoCharge coords grid) $ wholeGrid
    grid'       = zipWith (zipWith (+)) grid increment
    flashCoords = concatMap (\(r,c) -> zip [r,r..] c) . filter (not . null . snd)
                $ zip [ 0 .. length grid' ] (map (findIndices (>9)) grid')
    flashes'    = flashes + length flashCoords
    coords'     = filter (`notElem` flashCoords)
                . filter (\(r,c) -> r >= 0 && r < nRows && c >= 0 && c < nCols)
                . concatMap adjacentOctos $ flashCoords

day11 :: String -> Int
day11 inp = puzzle2
  where
    octoGrid = map (map digitToInt) . lines $ inp
    puzzle1 = octoStep 196 0 octoGrid []
    puzzle2 = octoStep' 0 0 octoGrid [] - 1

walk :: Map.Map String [String] -> [String] -> [String] -> [[String]] -> String -> [[String]]
walk graph visited path paths "end" = filter (not . null) $ ("end" : path) : paths
walk graph visited path paths node | null nodes = filter (not . null) paths
                                   | otherwise = concatMap (walk graph visited' path' paths) nodes
  where nodes = filter (`notElem` visited) . fromJust $ Map.lookup node graph
        path' = node : path
        visited' = if isLower . head $ node
                      then node : visited
                      else visited

nodeOptions :: [String] -> [String] -> [String]
nodeOptions path children = bigCaves ++ smallCaves
  where 
     children' = filter (/= "start") children
     bigCaves  = filter (isUpper . head) children'
     smallCaves' = filter (isLower . head) children'
     noTwice = not . any (>1) . map (\c -> count c path) $ smallCaves'
     smallCaves = filter (\c -> c == "end" || noTwice || c `notElem` path) smallCaves'

walk' :: Map.Map String [String] -> [String] -> [[String]] -> String -> [[String]]
walk' graph path paths "end" = filter (not . null) $ ("end" : path) : paths
walk' graph path paths node | null nodes = filter (not . null) paths
                            | otherwise = concatMap (walk' graph path' paths) nodes
  where children = fromJust $ Map.lookup node graph
        path' = node : path
        nodes = nodeOptions path' children

day12 :: String -> Int
day12 inp = paths' -- length paths
  where
    edges = concatMap ((\[a,b] -> [(a,[b]), (b,[a])]) . splitOn "-") . lines $ inp
    graph = Map.fromListWith (++) edges
    paths = walk' graph [] [[]] "start"
    paths' = length . filter (<2) . map (length . findIndices (>=2) 
           . map length . group . sort 
           . (filter (\n -> n /= "start" && n /= "end" && (isLower . head $ n)))) 
           $ paths
