module Generator() where

import System.Random
import Data.Set (Set, unions, fromList, member)
import Data.Map (Map, singleton, elems, (!), insert)
import Debug.Trace (trace)
import Data.List (intercalate)

main :: IO ()
main = do
    rng <- newStdGen
    layout <- return (createRandomLayout rng 0.1)
    --putStr (transformToListOfStrings layout)
    print(transformToListOfStrings layout)

transformToListOfStrings :: [Int] -> [String]
transformToListOfStrings xs = replaceZeros $ map (intercalate "") (chunksOf 9 (map show xs))
    where 
        replaceZeros :: [String] -> [String]
        replaceZeros = map (map (\c -> if c == '0' then '.' else c))

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

join :: Show a => String -> [a] -> String
join _   []     = ""
join _   (x:[]) = show x
join sep (x:xs) = show x ++ sep ++ join sep xs

fisherYatesStep :: RandomGen g => (Map Int a, g) -> (Int, a) -> (Map Int a, g)
fisherYatesStep (m, gen) (i, x) =
    ((insert j x . insert i (m ! j)) m, gen')
    where (j, gen') = randomR (0, i) gen
 
fisherYates :: RandomGen g => g -> [a] -> ([a], g)
fisherYates gen [] = ([], gen)
fisherYates gen l = 
    toElems $ foldl fisherYatesStep (initial (head l) gen) (numerate (tail l))
    where
        toElems (x, y) = (elems x, y)
        numerate = zip [1..]
        initial x gen = (singleton 0 x, gen)

posToIndex :: (Int, Int) -> Int
posToIndex (r, c) = r * 8 + c

getItem :: [Int] -> (Int, Int) -> Int
getItem layout pos = layout !! (posToIndex pos)

getRow :: [Int] -> (Int, Int) -> [Int]
getRow layout (r, _) = [getItem layout (r, i) | i <- [0..8]]

getColumn :: [Int] -> (Int, Int) -> [Int]
getColumn layout (_, c) = [getItem layout (i, c) | i <- [0..8]]

getBox :: [Int] -> (Int, Int) -> [Int]
getBox layout (r, c) =
    [getItem layout (cr, cc) | cr <- [sr..sr+2], cc <- [sc..sc+2]]
    where
        getStart = \ i -> (i `quot` 3) * 3
        sr = getStart r
        sc = getStart c

takenValues :: [Int] -> (Int, Int) -> Set Int
takenValues layout pos =
    unions (map fromList [getItems getRow, getItems getColumn, getItems getBox])
    where
        getItems = \ fn -> fn layout pos

isAllowed :: [Int] -> (Int, Int) -> Int -> Bool
isAllowed layout pos v = not (v `member` (takenValues layout pos))

createRandomLayout :: StdGen -> Float -> [Int]
createRandomLayout rng difficulty =
    createLayout baseLayout indices candidateValues
    where
        taker = take 81
        baseLayout = taker (repeat 0)
        (allIndices, _) = fisherYates rng [(x, y) | x <- [1..8], y <- [1..8]]
        numIndices = truncate (81 * (1.0 - difficulty))
        indices = take numIndices allIndices
        candidateValues = randomRs (1, 9) rng

createLayout :: [Int] -> [(Int, Int)] -> [Int] -> [Int]
createLayout layout [] _ = layout
createLayout layout indices candidates =
    createLayout newLayout (tail indices) newCandidates
    where
        index = head indices
        (newLayout, newCandidates) = fillPosition layout candidates index 9

fillPosition :: [Int] -> [Int] -> (Int, Int) -> Int -> ([Int], [Int])
fillPosition layout candidates pos attempts
    | attempts == 0 = (layout, candidates)
    | isAllowed layout pos candidate = (replacePos layout pos candidate, restCandidates)
    | otherwise = fillPosition layout restCandidates pos (attempts - 1)
    where
        candidate = head candidates
        restCandidates = tail candidates

replacePos :: [Int] -> (Int, Int) -> Int -> [Int]
replacePos layout pos value =
    [if curIndex == index then value else curValue | (curIndex, curValue) <- zip [0..] layout]
    where index = posToIndex pos

stringifyLayout :: [Int] -> String
stringifyLayout layout =
    join "\n" [join " " row | row <- rows]
    where rows = [getRow layout (i, 0) | i <- [0..8]]