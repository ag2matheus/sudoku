module FullCode (main) where
import Data.List 

--------------- import Definitions --------------------
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


--------------- Import Solver ----------------------------

valoresPossiveis :: Game -> Matrix Choices
valoresPossiveis =  map (map valores)
   where valores v | sudokuVoid v = sudokuValores
                   | otherwise    = [v]

-- Pega valores possíveis de cada casa, e monta todas os tabuleiros possíveis
organizaPossibilidades :: [[a]] -> [[a]]
organizaPossibilidades [] = [[]]
organizaPossibilidades (xs:xss) =  [y:ys | y <- xs, ys <- organizaPossibilidades xss]

reduzirOpcoes :: Row Choices -> Row Choices
reduzirOpcoes xss  =  [xs `removerDuplicatas` unicos | xs <- xss]
   where unicos = concat (filter sudokuUnico xss)

removerDuplicatas :: Choices -> Choices -> Choices
xs `removerDuplicatas` ys  | sudokuUnico xs = xs
                           | otherwise = xs \\ ys

eliminarOpcoes :: Matrix Choices -> Matrix Choices
eliminarOpcoes  =  eliminaErratas fileiras . eliminaErratas colunas . eliminaErratas quadrados
      where eliminaErratas f = f . map reduzirOpcoes . f

ajustaTipo :: Matrix [a] -> [Matrix a]
ajustaTipo =  organizaPossibilidades . map organizaPossibilidades                      

expandirEscolhas :: Matrix Choices -> [Matrix Choices]
expandirEscolhas sk  = [fileiras1 ++ [fileira1 ++ [c] : fileira2] ++ fileiras2 | c <- cs]
   where
      (fileiras1,fileira:fileiras2) = span (all sudokuUnico) sk
      (fileira1,cs:fileira2)        = span sudokuUnico fileira

buscarSolucoes :: Matrix Choices -> [Game]
buscarSolucoes sk | sudokuErrado sk          =  []
         | sudokuResolvido sk = ajustaTipo sk
         | otherwise          =  [g | sk' <- expandirEscolhas sk, g  <- buscarSolucoes (eliminarOpcoes sk')]


-- Devolve uma lista com possíveis soluções
resolveJogo :: Game -> [Game]
resolveJogo =  buscarSolucoes . eliminarOpcoes . valoresPossiveis

---------------------- Import Formatting ---------------------
-- Melhora o visual do tabuleiro
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

main :: IO ()
main = do
    putStrLn ("Este é o seu desafio, Boa Sorte!")
    putStrLn (visualizeBoard $ head $ resolveJogo gentle)