module Transformer (transform) where

type Game             = Matrix Value
type Matrix a         = [Row a]
type Row a            = [a]
type Choices          = [Value]
type Value            = Char

ajustaTipoVazio :: [Value] -> [Value]
ajustaTipoVazio [] = []
ajustaTipoVazio (x:xs)
    | x == '0'  = '.' : ajustaTipoVazio xs
    | otherwise = x : ajustaTipoVazio xs

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

dividePuzzle :: [Value] -> Game
dividePuzzle sdk = chunksOf 9 $ ajustaTipoVazio $ sdk

rawPuzzle :: [Value]
rawPuzzle = "060070080107000503050104090001000800600305009034206710000020000000907000000601000"

transform :: IO ()
transform = do 
    let puzzleLines = dividePuzzle rawPuzzle
    mapM_ putStrLn puzzleLines
    print (puzzleLines)

main :: IO ()
main = transform
