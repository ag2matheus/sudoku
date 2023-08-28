module TestWeb (main) where

import Network.HTTP.Simple

main :: IO ()
main = do
    response <- httpLBS "https://raw.githubusercontent.com/grantm/sudoku-exchange-puzzle-bank/master/medium.txt"
    putStrLn $ "Status code: " ++ show (getResponseStatusCode response)
    putStrLn $ "Response body: " ++ show (getResponseBody response)
