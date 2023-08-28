module PuzzleSorter (sorteia) where

import System.Random
import System.IO

------------------------------ Import Puzzle Sorter -----------------------------
--recebe dificuldade, retorna o filepath
setDificuldade :: String -> String
setDificuldade d | d == "easy" = "puzzles/easy.txt"
                 | d == "medium" = "puzzles/medium.txt"
                 | d == "hard" = "puzzles/hard.txt" 
                 | d == "diabolical" = "puzzles/diabolical.txt" 
                                  

-- Função para ler as linhas do arquivo e armazená-las em uma lista
readLines :: FilePath -> IO [String]
readLines arquivo = do
    corpo <- readFile arquivo
    return (lines corpo)

sorteia :: String -> IO String
sorteia dificuldade = do
    lines <- readLines $ setDificuldade dificuldade
    randomIndex <- randomRIO (0, length lines - 1)
    let randomSudoku = lines !! randomIndex
    putStrLn (randomSudoku)
    return randomSudoku

    -------------------------- transformer ------------

type Game             = Matrix Value
type Matrix a         = [Row a]
type Row a            = [a]
type Choices          = [Value]
type Value            = Char

--separa no puzzle selecionado, apenas a parte que representa o sudoku
selecionaJogo :: String -> [Value]
selecionaJogo sdk = take 81 $ drop 13 sdk

ajustaTipoVazio :: [Value] -> [Value]
ajustaTipoVazio [] = []
ajustaTipoVazio (x:xs)
    | x == '0'  = '.' : ajustaTipoVazio xs
    | otherwise = x : ajustaTipoVazio xs

separa :: Int -> [a] -> [[a]]
separa n [] =  []
separa n xs =  take n xs : separa n (drop n xs)

dividePuzzle :: [Value] -> Game
dividePuzzle sdk = separa 9 $ ajustaTipoVazio $ sdk

rawPuzzle :: [Value]
rawPuzzle = "ffca3f1df6fc 000072030060809040405000900750008000008000600000100027009000301030601090070940000  1.2"

transforma :: String -> String
transforma jogo = 
    let puzzleLines = dividePuzzle $ selecionaJogo $ jogo
        puzzleString = unlines puzzleLines
    in puzzleString
    putStrLn (puzzleString)


main :: IO ()
main = do 
    let dificuldade = "hard"
    randomSudoku <- sorteia dificuldade
    let transformedSudoku = transforma randomSudoku
    putStrLn transformedSudoku