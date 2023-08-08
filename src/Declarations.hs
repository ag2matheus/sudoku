module Declarations (isValid, solve) where

type Grid = Matrix Value

type Matrix a = [Row a]

type Row a = [a]

type Value = Char

type Sudoku = [[Int]]

isValid :: Sudoku -> Int -> (Int, Int) -> Bool
isValid sudoku num (row, col) =
    notElem num (getRow row) &&
    notElem num (getCol col) &&
    notElem num (getBlock (row `div` 3, col `div` 3))
    where
        getRow r = sudoku !! r
        getCol c = map (!! c) sudoku
        getBlock (r, c) = [sudoku !! (r * 3 + i) !! (c * 3 + j) | i <- [0..2], j <- [0..2]]

solve :: Sudoku -> Maybe Sudoku
solve sudoku
    | null emptyCells = Just sudoku
    | otherwise =
        case emptyCells of
            ((row, col):_) ->
                let possibleValues = filter (\num -> isValid sudoku num (row, col)) [1..9]
                in
                    foldl (\acc num -> acc <|> solve (updateSudoku num (row, col))) Nothing possibleValues
    where
        emptyCells = [(r, c) | r <- [0..8], c <- [0..8], sudoku !! r !! c == 0]
        updateSudoku num (row, col) = take row sudoku ++ [updateRow (sudoku !! row) num col] ++ drop (row + 1) sudoku
        updateRow row num col = take col row ++ [num] ++ drop (col + 1) row

exampleMatrix :: Matriz Int
exampleMatrix =         [[5, 3, 0, 0, 7, 0, 0, 0, 0]
         [6, 0, 0, 1, 9, 5, 0, 0, 0]
        , [0, 9, 8, 0, 0, 0, 0, 6, 0]
        , [8, 0, 0, 0, 6, 0, 0, 0, 3]
        , [4, 0, 0, 8, 0, 3, 0, 0, 1]
        , [7, 0, 0, 0, 2, 0, 0, 0, 6]
        , [0, 6, 0, 0, 0, 0, 2, 8, 0]
        , [0, 0, 0, 4, 1, 9, 0, 0, 5]
        , [0, 0, 0, 0, 8, 0, 0, 7, 9]]