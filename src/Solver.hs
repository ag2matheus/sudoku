
module Solver (resolveJogo) where

import Data.List
import Definitions

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
resolveJogo = buscarSolucoes . eliminarOpcoes . valoresPossiveis
