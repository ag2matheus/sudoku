module GetPuzzle (main) where

import System.Random
import Network.HTTP.Simple

main :: IO ()
main = do
    response <- httpLBS "https://raw.githubusercontent.com/grantm/sudoku-exchange-puzzle-bank/master/medium.txt"
    let content = getResponseBody response
        linesOfFile = lines content
        numLines = length linesOfFile
        randomIndex = randomRIO (0, numLines - 1)
    randomLineIndex <- randomIndex
    let randomLine = linesOfFile !! randomLineIndex
    putStrLn randomLine
