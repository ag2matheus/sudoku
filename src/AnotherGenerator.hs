module AnotherGenerator (test) where

import System.Random
import Data.Char
type Grid             = Matrix Value
type Matrix a         = [Row a]
type Row a            = [a]
type Value            = Char

test :: IO ()
test = do
    print(emptyGrid)
    putStrLn (randomChar)

emptyGrid :: Grid
emptyGrid = replicate 9 (replicate 9 '.')

randomChar :: IO Char
randomChar = do
    randomNum <- randomRIO (1, 9)
    return (intToDigit randomNum) -- Sorteia um número entre 1 e 9

-- ... Definições de rows, cols e boxs ...

-- Função para inserir números aleatórios respeitando as regras do Sudoku
{-insertRandomNumbers :: StdGen -> Int -> Grid -> Grid
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
-}

