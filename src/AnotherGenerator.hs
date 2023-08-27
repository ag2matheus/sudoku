module AnotherGenerator (test) where

import System.Random

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

emptyGrid :: [Char]
emptyGrid = replicate 81 '.'

-- ... Definições de rows, cols e boxs ...

-- Função para inserir números aleatórios respeitando as regras do Sudoku
insertRandomNumbers :: StdGen -> Int -> Grid -> Grid
insertRandomNumbers _ 0 grid = grid
insertRandomNumbers gen n grid =
    if null emptyPositions
        then grid
        else insertRandomNumbers newGen (n - 1) newGrid
    where
        emptyPositions = [(r, c) | r <- [0..8], c <- [0..8], empty (grid !! r !! c)]
        (index, newGen) = randomR (0, length emptyPositions - 1) gen
        (r, c) = emptyPositions !! index
        candidateValues = [v | v <- values, isAllowed grid (r, c) v]
        (chosenValueIndex, _) = randomR (0, length candidateValues - 1) newGen
        chosenValue = candidateValues !! chosenValueIndex
        newGrid = replacePos grid (r, c) chosenValue

-- Função para verificar se um valor é permitido em uma posição do grid
isAllowed :: Grid -> (Int, Int) -> Value -> Bool
isAllowed grid pos value =
    not (value `elem` takenValues)
    where
        takenValues = unions [fromList (getRow grid pos), fromList (getColumn grid pos), fromList (getBox grid pos)]

-- ... Definições de getRow, getColumn, getBox, takenValues, replacePos ...

-- Função para exibir o grid
visualizeGrid :: Grid -> String
visualizeGrid = unlines

test :: IO ()
test = do
    rng <- newStdGen
    let emptyGrid = replicate 9 (replicate 9 '.')
        n = 20  -- Quantidade de números aleatórios a serem inseridos
        finalGrid = insertRandomNumbers rng n emptyGrid
    putStrLn (finalGrid)
