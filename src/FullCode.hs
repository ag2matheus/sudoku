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

-- Caracteres Separadores
divisoriaHorizontal :: String
divisoriaHorizontal = replicate 11 '-'

divisoriaVertical :: Char
divisoriaVertical = '|'


addDivisoria :: String -> String
addDivisoria f = agregador [divisoriaVertical] grupos3
    where
        grupos3 = separa 3 f

agregador :: [a] -> [[a]] -> [a]
agregador _ [x] = x
agregador agg (x:xs) = x ++ agg ++ agregador agg xs

gameFormatado :: [String] -> String
gameFormatado sdk =
    unlines $ agregador [divisoriaHorizontal] grupos3
    where
        grupos3 = separa 3 agrupamentos
        agrupamentos = map addDivisoria sdk

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

teste :: Game
teste = [".76..94.."
        ,"...8.1..7"
        ,"3....9..."
        ,"61.3.7.8."
        ,"...9...."
        ,".2.1.8.34"
        ,"5....6..."
        ,"9..24...."
        ,"..16.79.."]

teste2 :: Game
teste2 =["54.9....6","...2.7.8",".7.43...",".8....9.",".9.4.3..","6.2...5.","...21.8.","1.76....","8...4.27"]

    --["540900006","000002708","070430000","080000901","009040300","602000050","000021080","107600000","800004027"]
teste3 :: Game
teste3 = [".6..7..8.","1.7...5.3",".5.1.4.9.","..1...8..","6..3.5..9",".342.671.","....2....","...9.7...","...6.1..."]

main :: IO ()
main = do
    let puzzle = teste3
        possibilidades = (length $ resolveJogo puzzle)
    putStrLn ("Este é o seu desafio, Boa Sorte!")
    putStrLn (gameFormatado $ puzzle)
    putStrLn ("Esta é uma das " ++ show possibilidades ++ " solucoes possiveis!")
    putStrLn (gameFormatado $ head $ resolveJogo puzzle)