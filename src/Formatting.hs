module Formatting (visualizeBoard) where

-- Printing Formated Table

-- Função para visualizar o tabuleiro com barras separando os grupos de 3 números
visualizeBoard :: [String] -> String
visualizeBoard board =
    unlines $ separate [lineSeparator] groupedRows
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