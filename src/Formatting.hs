module Formatting (gameFormatado) where

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