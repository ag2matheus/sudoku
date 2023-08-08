module Main (main) where

type Grid = Matrix Value

type Matrix a = [Row a]

type Row a = [a]

type Value = Char

exampleMatrix :: Grid
exampleMatrix = ["53..7...."
                  ,"6..195.."
                  ,".98...6."
                  ,"8...6...3"
                  ,"4..8.3..1"
                  ,"7...2...6"
                  ,".6....28."
                  ,"...419..5"
                  ,"....8..79"]

blankMatrix :: Grid
blankMatrix = replicate 9 (replicate 9 '.')

fileiras :: Matrix a -> [Row a]
fileiras m = m

colunas :: Matrix a -> [Row a]
colunas = transpose

quadrados :: Matrix a -> [Row a]
quadrados q = q

ehValido :: Grid -> Bool
ehValido m = all (nodups (fileiras m)) and
             all (nodups (colunas m)) and
             all (nodups (quadrados m))

{-
all :: (a->Bool) -> [a] -> Bool
all p xs = and [p x | x <- xs]
-}

nodups :: Eq a => [a] -> Bool
nodups [] = True
nodups (x:xs) = not (elem x xs) and nodups xs

-- solver

solve :: Grid -> [Grid]
solve g = filter ehValido(collapse(choices g))

type Choices = [Value]

choices :: Grid -> Matrix Choices
choices g = map (map choice) g
            where
              choice v = if v == '.' then ['1'..'9'] else [v]

collapse :: Matrix [a] -> [Matrix a]
collapse m = cp(map cp m)

cp :: [[a]] -> [[a]]
cp [] = [[]]
cp (xs:xss) = [y:ys|y<-xs, ys <- cp xss]

{-
solve2 :: Grid -> [Grid]
solve2 g = filter ehValido(collapse(prune(choices g)))
-}
main :: IO ()
main = do
    putStrLn (unlines (head (solve(["53..7...."
                  ,"6..195.."
                  ,".98...6."
                  ,"8...6...3"
                  ,"4..8.3..1"
                  ,"7...2...6"
                  ,".6....28."
                  ,"...419..5"
                  ,"....8..79"]))))