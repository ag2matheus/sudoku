module TestGenerator (main) where

import System.Random

type IntGrid = [[Int]]

insertRandom :: [Int] -> IO [Int]
insertRandom list = do
    rng <- newStdGen
    let (index, newRNG) = randomR (0, length list - 1) rng
        (value, _) = randomR (1, 9) newRNG
        (before, after) = splitAt index list
    return $ before ++ [value] ++ tail after

isInRow :: IntGrid -> Int -> Int -> Int -> Bool
isInRow grid row col num = num `elem` rowValues
  where
    rowValues = grid !! row

isInColumn :: IntGrid -> Int -> Int -> Int -> Bool
isInColumn grid row col num = num `elem` colValues
  where
    colValues = map (!! col) grid

isInSquare :: IntGrid -> Int -> Int -> Int -> Bool
isInSquare grid row col num = num `elem` squareValues
  where
    squareValues = [grid !! r !! c | r <- rows, c <- cols]
    rows = [r * 3 + rowStart | r <- [0..2]]
    cols = [c * 3 + colStart | c <- [0..2]]
    rowStart = (row `div` 3) * 3
    colStart = (col `div` 3) * 3

isSafe :: IntGrid -> Int -> Int -> Int -> Bool
isSafe grid row col num =
    not (isInRow grid row col num || isInColumn grid row col num || isInSquare grid row col num)

main :: IO ()
main = do
    let emptyList = replicate 81 0
    newList <- insertRandom emptyList
    print newList