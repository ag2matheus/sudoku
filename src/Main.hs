module Main (main) where

import Definitions
import Solver
import Formatting

gentle :: Game
gentle =  [".1.42...5",
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
    putStrLn (gameFormatado $ head $ resolveJogo gentle)