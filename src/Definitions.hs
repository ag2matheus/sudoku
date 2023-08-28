module Definitions (Game, 
                    Matrix, 
                    Row, 
                    Choices,
                    sudokuVoid,
                    sudokuVazio,
                    sudokuValores,
                    sudokuUnico,
                    sudokuSemRep,
                    sudokuErrado,
                    sudokuResolvido,
                    fileiras,
                    colunas,
                    quadrados) where

import Data.List

-- Tipos criados
type Game             = Matrix Value
type Matrix a         = [Row a]
type Row a            = [a]
type Choices          = [Value]
type Value            = Char

-- Helpers Functions - Checa valores das Casas
-- Helpers para as casas
sudokuValores :: [Value]
sudokuValores = ['1'..'9']

sudokuVoid :: Value -> Bool
sudokuVoid =  (== '.')

sudokuUnico :: [a] -> Bool
sudokuUnico [_]  =  True
sudokuUnico _  =  False

-- Helpers para o Tabuleiro
sudokuResolvido :: Matrix Choices -> Bool
sudokuResolvido =  all (all sudokuUnico)

sudokuErrado :: Matrix Choices -> Bool
sudokuErrado sk =  sudokuVazio sk || not (sudokuSemRep sk)

sudokuSemRep :: Matrix Choices -> Bool
sudokuSemRep sk =  all consistent (fileiras sk) && all consistent (colunas sk) && all consistent (quadrados sk)
    where consistent :: Row Choices -> Bool
          consistent =  semRepeticao . concat . filter sudokuUnico

sudokuVazio :: Matrix Choices -> Bool
sudokuVazio =  any (any null)

--Verifica repetições em lista
semRepeticao :: Eq a => [a] -> Bool
semRepeticao [] =  True
semRepeticao (x:xs) =  not (elem x xs) && semRepeticao xs


-- Define estruturas principais do Sudoku
fileiras :: Matrix a -> [Row a]
fileiras f = f

colunas :: Matrix a -> [Row a]
colunas = transpose

-- cria lista de listas, onde cada elemento lista é um quadrado
quadrados :: Matrix a -> [Row a]
quadrados =  reagrupar . map colunas . agregar3
    where
        agregar3 = criarBlocos . map criarBlocos
        criarBlocos = separa 3
        reagrupar = map concat . concat

separa :: Int -> [a] -> [[a]]
separa n [] =  []
separa n xs =  take n xs : separa n (drop n xs)

------ Sudoku Examples --------
-- Primeiro exemplo suave do sudoku.org.uk:
gentle                :: Game
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
diabolical            :: Game
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
unsolvable            :: Game
unsolvable            =  ["1..9.7..3",
                          ".8.....7.",
                          "..9...6..",
                          "..72.94..",
                          "41.....95",
                          "..85.43..",
                          "..3...7..",
                          ".5.....4."]