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
    , day10
    , day11
    , day12
    , day13
    , day14
    , day15
    , day16
    ) where

import Control.Applicative
import Debug.Trace
import Data.List
import Data.List.Split
import Data.Char (digitToInt, intToDigit, isUpper, isLower, isSpace, isLetter, isDigit)
import Data.Maybe (mapMaybe, catMaybes, isNothing, isJust, fromJust)
import Data.Tuple (swap)
import Data.Ord (comparing)
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

errorScore :: Char -> Int
errorScore ')' = 3
errorScore ']' = 57
errorScore '}' = 1197
errorScore '>' = 25137
errorScore  _  = 0

closingPoints :: Char -> Int
closingPoints ')' = 1
closingPoints ']' = 2
closingPoints '}' = 3
closingPoints '>' = 4
closingPoints  _  = 0

isOpening :: Char -> Bool
isOpening = flip elem "([{<"

isClosing :: Char -> Bool
isClosing = flip elem ">}])"

close :: Char -> Char
close '(' = ')'
close '[' = ']'
close '{' = '}'
close '<' = '>'
close  _  = ' '

checkLine :: String -> String -> Char
checkLine c l | null l = ' '
              | null c && isOpening o = checkLine [close o] (tail l) 
              | null c && isClosing o = o
              | isOpening o = checkLine (close o : c) (tail l)
              | isClosing o && (o == head c) = checkLine (tail c) (tail l)
              | otherwise = o
  where
    o = head l

completeLine :: String -> String -> String
completeLine c l | null l = c
                 | null c && isOpening o = completeLine [close o] (tail l) 
                 | null c && isClosing o = ""
                 | isOpening o = completeLine (close o : c) (tail l)
                 | isClosing o && (o == head c) = completeLine (tail c) (tail l)
                 | otherwise = ""
  where
    o = head l

day10 :: String -> Int
day10 inp = points
  where
    score = sum . map errorScore . filter isClosing . map (checkLine "") 
          . lines $ inp
    points' = filter (>0) . map (foldl (\a p -> a * 5 + p) 0 
            . map closingPoints . completeLine "") . lines $ inp
    points = sort points' !! (length points' `div` 2)

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

fold :: [(Int, Int)] -> (String, Int) -> [(Int, Int)]
fold points ("y", line) = nub (pointsAbove ++ foldedPoints)
  where
    pointsAbove  = filter ((<line) . snd) points
    pointsBelow  = filter ((>line) . snd) points
    foldedPoints = map (\(x,y) -> (x, line - (y - line))) pointsBelow
fold points ("x", line) = nub (pointsLeft ++ foldedPoints)
  where
    pointsLeft  = filter ((<line) . fst) points
    pointsRight = filter ((>line) . fst) points
    foldedPoints = map (\(x,y) -> (line - (x - line), y)) pointsRight
fold points     _       = points

addPoint :: String -> Int -> String
addPoint line i = take i line ++ "#" ++ drop (i+1) line

day13 :: String -> Int
day13 inp = visible
  where
    manual = lines inp
    (points',instr') = (\(p,i) -> (p, tail i)) . splitAt (fromJust . elemIndex "" $ manual) $ manual
    points = map ((\[x,y] -> (x,y)) . map read . splitOn ",") points' :: [(Int,Int)]
    instr = map ((\[x,y] -> (x, read y)) . splitOn "=" . last . words) instr' :: [(String, Int)]
    visible = length . fold points . head $ instr
    finalPoints = foldl fold points instr
    pointsPerLine = groupBy (\p1 p2 -> snd p1 == snd p2) . map swap . sort . map swap $ finalPoints
    numCols = (+1) . maximum . map fst $ finalPoints
    numRows = (+1) . maximum . map snd $ finalPoints
    code = map (foldl (\l (p,_) -> addPoint l p) (replicate numCols ' ')) pointsPerLine

polym :: Map.Map String Char -> [Char] -> [Char]
polym rules ""       = ""
polym rules [b]      = [b]
polym rules (a : b : ps) = a : c : polym rules (b:ps)
  where
    c = fromJust $ Map.lookup [a,b] rules

polymToPairs :: String -> [String]
polymToPairs [b] = []
polymToPairs (a:b:ps) = [a,b] : (polymToPairs (b:ps))

polyPatterns :: Map.Map String Char -> String -> [String]
polyPatterns rules [a,b] = [[a,c], [c,b]]
  where
    c = fromJust $ Map.lookup [a,b] rules

polymStep :: Map.Map String Char -> Map.Map String Int -> Map.Map Char Int 
          -> (Map.Map String Int, Map.Map Char Int)
polymStep rules poly occur = (poly', occur')
  where
    occurs = concatMap (\(p,o) -> replicate o p) . Map.toList . Map.filter (>0) $ poly
    occur' = foldl (flip (Map.adjust (+1))) occur $ map (\o -> fromJust 
           $ Map.lookup o rules) occurs
    updateOccurs = concatMap (polyPatterns rules) occurs
    poly' = foldl (flip (Map.adjust (+ 1))) 
                  (Map.fromList $ zip (Map.keys rules) [0,0 .. ]) 
                  updateOccurs 

polymStep' :: Int -> Map.Map String Char -> Map.Map String Int -> Map.Map Char Int 
          -> (Map.Map String Int, Map.Map Char Int)
polymStep' 0  rules poly occur = (poly, occur)
polymStep' i rules poly occur = polymStep' (i-1) rules poly' occur'
  where
    ply = Map.filter (>0) poly
    occur' = foldl (\o (k,v) -> Map.adjust (+v) k o) occur 
           . map (\(p,o) -> (fromJust $ Map.lookup p rules, o)) 
           . Map.toList $ ply
    poly' = Map.fromListWith (+) . concatMap (\(k,v) -> zip (polyPatterns rules k) [v,v ..]) . Map.toList $ ply

day14 :: String -> Int
day14 inp = maximum ocr - minimum ocr
  where
    inp' = lines inp
    template' = head inp'
    template = polymToPairs template'
    rules = Map.fromList . map ((\[p,r] -> (p,head r)) . splitOn " -> ") $ drop 2 inp'
    temp = Map.mapWithKey (\p o -> count p template) 
         $ Map.fromList $ zip (Map.keys rules) [0,0 .. ]
    occur = Map.mapWithKey (\p o -> count p template') . Map.fromList 
          $ zip (nub . concat . Map.keys $ rules) [0,0 .. ]
    nSteps = 40
    (polyms, occurs) = polymStep' nSteps rules temp occur
    ocr = map snd . Map.toList $ occurs

    --run = polym rules template'
    --polymer = foldl (\temp _ -> polym rules temp) template' [1 .. nSteps]
    -- occur = map length . group . sort $ polymer
    -- naive = maximum occur - minimum occur

riskAt :: [[Int]] -> (Int,Int) -> Int
riskAt g i =  g !! fst i !! snd i

tile :: [[Int]] -> (Int, Int) -> Int
tile grid (r,c) = t !! r' !! c'
  where
    gd = length grid
    t = [take 5 . drop d . cycle $ [0 .. 8] | d <- [0 .. 4]]
    r' = div r gd
    c' = div c gd

riskAt' :: [[Int]] -> (Int,Int) -> Int
riskAt' g i = (cycle [ 1 .. 9 ]) !! (r + t - 1)
  where 
    t = tile g i
    gd = length g
    r' = cycle [ 0 .. (gd - 1) ] !! fst i
    c' = cycle [ 0 .. (gd - 1) ] !! snd i
    r = g !! r' !! c'

chitonStep :: [[Int]] -> (Int, Int) -> Map.Map (Int,Int) Int -> Set.Set (Int,Int) -> Int
chitonStep grid target stack visited
    | idx == target = cost
    | otherwise = chitonStep grid target stack' visited'
  where
    this = minimumBy (comparing snd) . Map.toList $ stack
    cost = snd this
    idx = fst this
    --gd = length grid
    gd = (*5) . length $ grid
    (r,c) = idx
    cross = filter (\(r',c') -> (r' >= 0) && (c' >= 0) && (r' < gd) && (c' < gd) 
                             && Set.notMember (r',c') visited) 
                   [ (r + 1, c), (r - 1, c), (r, c + 1), (r, c - 1) ]
    stack' = Map.unionWith min (Map.delete idx stack) . Map.fromList 
           . map (\i -> (i, riskAt' grid i + cost)) $ cross
    visited' = Set.insert idx visited

day15 :: String -> Int
day15 inp = path
  where
    grid = map (map digitToInt) . lines $ inp
    s = (0,0)
    -- t = (subtract 1 . length $ grid, subtract 1 . length . head $ grid)
    t = (subtract 1 . (*5) . length $ grid, subtract 1 . (*5) . length . head $ grid)
    path = chitonStep grid t (Map.fromList [(s, 0)]) Set.empty

toBin :: Char -> String
toBin '0' = "0000"
toBin '1' = "0001"
toBin '2' = "0010"
toBin '3' = "0011"
toBin '4' = "0100"
toBin '5' = "0101"
toBin '6' = "0110"
toBin '7' = "0111"
toBin '8' = "1000"
toBin '9' = "1001"
toBin 'A' = "1010"
toBin 'B' = "1011"
toBin 'C' = "1100"
toBin 'D' = "1101"
toBin 'E' = "1110"
toBin 'F' = "1111"
toBin  _  = "" -- error "You screwed up day 16!"

data Packet = LengthOperator {typeID :: Int, version :: Int, len :: Int, subPackets :: [Packet]}
            | CountOperator  {typeID :: Int, version :: Int, cnt :: Int, subPackets :: [Packet]}
            | LiteralValue   {typeID :: Int, version :: Int, value :: Int}
    deriving (Eq, Show)

trData' :: String -> String
trData' ('1':b1:b2:b3:b4:bs) = [b1,b2,b3,b4] ++ trData' bs
trData' ('0':b1:b2:b3:b4:bs) = [b1,b2,b3,b4]
trData'          _           = ""

trData :: String -> (Int, Int)
trData d = (d', i')
    where d'' = trData' d
          d'  = toDec d''
          i'  = round $ (realToFrac . length $ d'') / 4 * 5

mkPacket :: String -> [Packet]
mkPacket (v1:v2:v3:'1':'0':'0':d) = (LiteralValue 4 v d') 
                                  : (mkPacket . drop i' $ d)
    where (d', i') = trData d
          v = toDec [v1,v2,v3]
mkPacket (v1:v2:v3:t1:t2:t3:'0':d) = (LengthOperator t v l d') 
                                   : (mkPacket . drop (15 + l) $ d)
  where t = toDec [t1,t2,t3]
        v = toDec [v1,v2,v3]
        l = toDec . take 15 $ d
        d' = mkPacket . take l . drop 15 $ d
mkPacket (v1:v2:v3:t1:t2:t3:'1':d) = (CountOperator t v n d') 
                                   : (drop n . mkPacket . drop 11 $ d)
  where t = toDec [t1,t2,t3]
        v = toDec [v1,v2,v3]
        n = toDec . take 11 $ d
        d' = take n . mkPacket . drop 11 $ d
mkPacket _ = []

sumVersions :: Packet -> Int
sumVersions pkg | typeID pkg == 4 = version pkg
                | otherwise = (version pkg) + (sum . map sumVersions . subPackets $ pkg)

packFun :: Int -> ([Int] -> Int)
packFun 0 = sum
packFun 1 = product
packFun 2 = minimum
packFun 3 = maximum
packFun 5 = \[a, b] -> if a > b then 1 else 0
packFun 6 = \[a, b] -> if a < b then 1 else 0
packFun 7 = \[a, b] -> if a == b then 1 else 0
packFun _ = const 0

processPacket :: Packet -> Int
processPacket pkg | t == 4 = value pkg
                  | otherwise = v
  where 
    t = typeID pkg
    v = packFun t . map processPacket . subPackets $ pkg

day16' :: [String] -> [Int]
day16' inp = vv
  where
    ts = map (concatMap toBin) inp
    pkgs = map mkPacket ts
    vv = map (sum . map sumVersions) pkgs

day16 :: String -> Int
day16 inp = vals -- v
  where
    t   = concatMap toBin inp
    pkg = mkPacket t
    v = sum . map sumVersions $ pkg
    vals = sum . map processPacket $ pkg
