module Main (main) where

--import Solver
--import Generator

import System.Random
import Data.Set (Set, unions, fromList, member)
import Data.Map (Map, singleton, elems, (!), insert)
import Debug.Trace (trace)
import Data.List (intercalate, transpose, (\\))

main :: IO ()
main = do
   -- rng <- newStdGen
    --layout <- return (createRandomLayout rng 0.5)
    let inicialPuzzle = baby --transformToListOfStrings layout
    let finalBoard =   head $ solve4 gentle
    let possibilidades = (length $ solve4 baby) - 1
        --visualizedBoard = visualizeBoard finalBoard
    --putStrLn (finalBoard)
    putStrLn ("Este é o seu desafio, Boa Sorte!")
    putStr (visualizeBoard finalBoard)
    --putStrLn ("\n Esta é uma das " ++ show possibilidades ++ " solucoes possiveis do Sudoko")
    putStr (visualizeBoard $ head $ solve4 baby)
   -- print (solve4 baby)



-- Definitions File
type Grid             = Matrix Value
type Matrix a         = [Row a]
type Row a            = [a]
type Value            = Char


boxsize               :: Int
boxsize               =  3

values                :: [Value]
values                =  ['1'..'9']

empty                 :: Value -> Bool
empty                 =  (== '.')

single                :: [a] -> Bool
single [_]            =  True
single _              =  False

--- Example grids File

-- Solvable only using the basic rules:
easy                  :: Grid
easy                  =  ["2....1.38",
                          "........5",
                          ".7...6...",
                          ".......13",
                          ".981..257",
                          "31....8..",
                          "9..8...2.",
                          ".5..69784",
                          "4..25...."]
baby :: Grid
baby = ["..2...9..",
        ".6...3...",
        ".4.......",
        ".75......",
        "...38.2..",
        ".8......6",
        "..38..1..",
        ".9.6..5..",
        "1........"]

-- Primeiro exemplo suave do sudoku.org.uk:
gentle                :: Grid
gentle                =  [".1.42...5",
                          "..2.71.39",
                          ".......4.",
                          "2.71....6",
                          "....4....",
                          "6....74.3",
                          ".7.......",
                          "12.73.5..",
                          "3...82.7."]

-- Primeiro exemplo diabólico:
diabolical            :: Grid
diabolical            =  [".9.7..86.",
                          ".31..5.2.",
                          "8.6......",
                          "..7.5...6",
                          "...3.7...",
                          "5...1.7..",
                          "......1.9",
                          ".2.6..35.",
                          ".54..8.7."]

-- Primeiro exemplo "insolúvel" (requer retrocesso):
unsolvable            :: Grid
unsolvable            =  ["1..9.7..3",
                          ".8.....7.",
                          "..9...6..",
                          "..72.94..",
                          "41.....95",
                          "..85.43..",
                          "..3...7..",
                          ".5.....4."]



--------

-- Extracting rows is trivial:
rows                  :: Matrix a -> [Row a]
rows                  =  id

-- We also have, trivially, that rows . rows = id.  This property (and
-- similarly for cols and boxs) will be important later on.
cols                  :: Matrix a -> [Row a]
cols                  =  transpose

-- Example: cols [[1,2],[3,4]] = [[1,3],[2,4]].  Exercise: define
-- transpose, without looking at the library definition.
boxs                  :: Matrix a -> [Row a]
boxs                  =  unpack . map cols . pack
                        where
                           pack   = split . map split
                           split  = chop boxsize
                           unpack = map concat . concat
chop                  :: Int -> [a] -> [[a]]
chop n []             =  []
chop n xs             =  take n xs : chop n (drop n xs)


------------------------------
-- Generator
{-
main :: IO ()
main = do
    rng <- newStdGen
    layout <- return (createRandomLayout rng 0.1)
    --putStr (transformToListOfStrings layout)
    print(transformToListOfStrings layout)-}

transformToListOfStrings :: [Int] -> [String]
transformToListOfStrings xs = replaceZeros $ map (intercalate "") (chunksOf 9 (map show xs))
    where 
        replaceZeros :: [String] -> [String]
        replaceZeros = map (map (\c -> if c == '0' then '.' else c))

--chunksOf :: Int -> [a] -> [[a]]
--chunksOf _ [] = []
--chunksOf n xs = take n xs : chunksOf n (drop n xs)

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



-- Validity checking
-----------------

-- Now let us turn our attention from matrices to Sudoku grids.  A grid
-- is valid if there are no duplicates in any row, column or box:
valid                 :: Grid -> Bool
valid g               =  all nodups (rows g) &&
                        all nodups (cols g) &&
                        all nodups (boxs g)

nodups                :: Eq a => [a] -> Bool
nodups []             =  True
nodups (x:xs)         =  not (elem x xs) && nodups xs

-- A basic solver
--------------

-- The function choices replaces blank squares in a grid by all possible
-- values for that square, giving a matrix of choices:
type Choices          =  [Value]
choices               :: Grid -> Matrix Choices
choices               =  map (map choice)
                        where
                           choice v = if empty v then values else [v]

cp                    :: [[a]] -> [[a]]
cp []                 =  [[]]
cp (xs:xss)           =  [y:ys | y <- xs, ys <- cp xss]

collapse              :: Matrix [a] -> [Matrix a]
collapse              =  cp . map cp

solve                 :: Grid -> [Grid]
solve                 =  filter valid . collapse . choices

------

prune                 :: Matrix Choices -> Matrix Choices
prune                 =  pruneBy boxs . pruneBy cols . pruneBy rows
                         where pruneBy f = f . map reduce . f
reduce                :: Row Choices -> Row Choices
reduce xss            =  [xs `minus` singles | xs <- xss]
                         where singles = concat (filter single xss)
minus                 :: Choices -> Choices -> Choices
xs `minus` ys         =  if single xs then xs else xs \\ ys

solve2                :: Grid -> [Grid]
solve2                =  filter valid . collapse . prune . choices

-------

solve3                :: Grid -> [Grid]
solve3                =  filter valid . collapse . fix prune . choices
fix                   :: Eq a => (a -> a) -> a -> a
fix f x               =  if x == x' then x else fix f x'
                         where x' = f x
-----------

complete              :: Matrix Choices -> Bool
complete              =  all (all single)

--Similarly, a matrix is "void" if some square contains no choices:

void                  :: Matrix Choices -> Bool
void                  =  any (any null)

--In turn, we use the term "safe" for matrix for which all rows,
--columns and boxes are consistent, in the sense that they do not
--contain more than one occurrence of the same single choice:

safe                  :: Matrix Choices -> Bool
safe cm               =  all consistent (rows cm) &&
                         all consistent (cols cm) &&
                         all consistent (boxs cm)
consistent            :: Row Choices -> Bool
consistent            =  nodups . concat . filter single

--Finally, a matrix is "blocked" if it is void or unsafe:

blocked               :: Matrix Choices -> Bool
blocked m             =  void m || not (safe m)

solve4                :: Grid -> [Grid]
solve4                =  search . prune . choices
search                :: Matrix Choices -> [Grid]
search m
 | blocked m          =  []
 | complete m         =  collapse m
 | otherwise          =  [g | m' <- expand m
                            , g  <- search (prune m')]

--The function expand behaves in the same way as collapse, except that
--it only collapses the first square with more than one choice:

expand                :: Matrix Choices -> [Matrix Choices]
expand m              =
   [rows1 ++ [row1 ++ [c] : row2] ++ rows2 | c <- cs]
   where
      (rows1,row:rows2) = break (any (not . single)) m
      (row1,cs:row2)    = break (not . single) row


--------
-- Printing Formated Table

-- Função para visualizar o tabuleiro com barras separando os grupos de 3 números
visualizeBoard :: [String] -> String
visualizeBoard board =
    unlines $ intercalate [lineSeparator] groupedRows
    where
        groupedRows = chunksOf 3 formattedRows
        formattedRows = map formatRow board

-- Função para formatar uma linha com as barras separando os grupos de 3 números
formatRow :: String -> String
formatRow row = separate [cellSeparator] groupedCells
    where
        groupedCells = chunksOf 3 row

-- Separadores
lineSeparator :: String
lineSeparator = replicate 11 '-'

cellSeparator :: Char
cellSeparator = '|'

-- Função para dividir uma lista em grupos de tamanho n
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

-- Função de ajuda para juntar elementos de uma lista em uma string com separador
separate :: [a] -> [[a]] -> [a]
separate _ [x] = x
separate sep (x:xs) = x ++ sep ++ separate sep xs
