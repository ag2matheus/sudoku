module Transformer (transform) where

type Game             = Matrix Value
type Matrix a         = [Row a]
type Row a            = [a]
type Choices          = [Value]
type Value            = Char

----------------------------- Import Transformer -----------------------------------------
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

transform :: IO ()
transform = do 
    let puzzleLines = dividePuzzle $ selecionaJogo $ rawPuzzle
    print (puzzleLines)

main :: IO ()
main = transform
