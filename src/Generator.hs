module Generator (generate) where

import System.Random


-- Tipos
type Grid = Matrix Value
type Matrix a = [Row a]
type Row a = [a]
type Value = Char

-- Basic definitions 
-- -----------------

boxsize :: Int
boxsize = 3

values :: [Value]
values = ['1'..'9']

empty :: Value -> Bool
empty = (== '.')

single :: [a] -> Bool
single [_] = True
single _ = False

-- Funções

-- Função para criar um tabuleiro vazio
emptyGrid :: Grid
emptyGrid = replicate 9 (replicate 9 '.')

-- Função para imprimir um tabuleiro
printGrid :: Grid -> IO ()
printGrid = mapM_ putStrLn

-- Função para verificar se um número é válido em uma posição específica do tabuleiro
isValid :: Grid -> Position -> Char -> Bool
isValid grid (row, col) num =
  notElem num (getRow row) &&
  notElem num (getCol col) &&
  notElem num (getBox boxRow boxCol)
  where
    getRow r = grid !! r
    getCol c = [grid !! r !! c | r <- [0..8]]
    boxRow = row `div` 3
    boxCol = col `div` 3
    getBox r c = [grid !! i !! j | i <- [r*3..r*3+2], j <- [c*3..c*3+2]]

-- Função para preencher uma célula no tabuleiro
fillCell :: Grid -> Position -> Char -> Grid
fillCell grid (row, col) num = 
  take row grid ++
  [take col (grid !! row) ++ num : drop (col + 1) (grid !! row)] ++
  drop (row + 1) grid

-- Função para gerar um tabuleiro solucionável aleatoriamente
generateSolvableGrid :: StdGen -> Grid
generateSolvableGrid gen = fillRandomCells emptyGrid 0
  where
    fillRandomCells :: Grid -> Int -> Grid
    fillRandomCells grid count
      | count >= 17 = grid
      | otherwise =
          let (row, newGen1) = randomR (0, 8) gen
              (col, newGen2) = randomR (0, 8) newGen1
              (num, newGen3) = randomR values newGen2
          in if isValid grid (row, col) num
             then fillRandomCells (fillCell grid (row, col) num) (count + 1)
             else fillRandomCells grid count

-- Função para imprimir um tabuleiro solucionável gerado
printSolvableGrid :: IO ()
printSolvableGrid = do
  gen <- newStdGen
  let solvableGrid = generateSolvableGrid gen
  printGrid solvableGrid

-- Main
generate :: IO ()
generate = printSolvableGrid