
module Solver (solve, solve2, solve3, solve4, easy, gentle, diabolical) where
-- Library file
-- ------------

-- We use a few things from the list library:
import Data.List 

-- Basic declarations
-- ------------------

-- We begin with some basic declarations, using the terminology of
-- sudoku.org.uk.  Although the declarations do not enforce it,
-- we will only consider non-empty square matrices with a multiple
-- of boxsize (defined in the next section) rows.  This assumption
-- is important for various properties that we rely on.
type Grid             = Matrix Value
type Matrix a         = [Row a]
type Row a            = [a]
type Value            = Char

-- Basic definitions 
-- -----------------

boxsize               :: Int
boxsize               =  3

values                :: [Value]
values                =  ['1'..'9']

empty                 :: Value -> Bool
empty                 =  (== '.')

single                :: [a] -> Bool
single [_]            =  True
single _              =  False

-- Example grids
-- -------------

-- Solvable only using the basic rules:
easy                  :: Grid
easy                  =  ["2....1.38",
                          "........5",
                          ".7...6...",
                          ".......13",
                          ".981..257",
                          "31....8..",
                          "9..8...2.",
                          ".5..69784",
                          "4..25...."]

-- Primeiro exemplo suave do sudoku.org.uk:
gentle                :: Grid
gentle                =  [".1.42...5",
                          "..2.71.39",
                          ".......4.",
                          "2.71....6",
                          "....4....",
                          "6....74.3",
                          ".7.......",
                          "12.73.5..",
                          "3...82.7."]

-- Primeiro exemplo diabólico:
diabolical            :: Grid
diabolical            =  [".9.7..86.",
                          ".31..5.2.",
                          "8.6......",
                          "..7.5...6",
                          "...3.7...",
                          "5...1.7..",
                          "......1.9",
                          ".2.6..35.",
                          ".54..8.7."]

-- Primeiro exemplo "insolúvel" (requer retrocesso):
unsolvable            :: Grid
unsolvable            =  ["1..9.7..3",
                          ".8.....7.",
                          "..9...6..",
                          "..72.94..",
                          "41.....95",
                          "..85.43..",
                          "..3...7..",
                          ".5.....4."]



--------

-- Extracting rows is trivial:
rows                  :: Matrix a -> [Row a]
rows                  =  id

-- We also have, trivially, that rows . rows = id.  This property (and
-- similarly for cols and boxs) will be important later on.
cols                  :: Matrix a -> [Row a]
cols                  =  transpose

-- Example: cols [[1,2],[3,4]] = [[1,3],[2,4]].  Exercise: define
-- transpose, without looking at the library definition.
boxs                  :: Matrix a -> [Row a]
boxs                  =  unpack . map cols . pack
                        where
                           pack   = split . map split
                           split  = chop boxsize
                           unpack = map concat . concat
chop                  :: Int -> [a] -> [[a]]
chop n []             =  []
chop n xs             =  take n xs : chop n (drop n xs)

-- Validity checking
-----------------

-- Now let us turn our attention from matrices to Sudoku grids.  A grid
-- is valid if there are no duplicates in any row, column or box:
valid                 :: Grid -> Bool
valid g               =  all nodups (rows g) &&
                        all nodups (cols g) &&
                        all nodups (boxs g)

nodups                :: Eq a => [a] -> Bool
nodups []             =  True
nodups (x:xs)         =  not (elem x xs) && nodups xs

-- A basic solver
--------------

-- The function choices replaces blank squares in a grid by all possible
-- values for that square, giving a matrix of choices:
type Choices          =  [Value]
choices               :: Grid -> Matrix Choices
choices               =  map (map choice)
                        where
                           choice v = if empty v then values else [v]

cp                    :: [[a]] -> [[a]]
cp []                 =  [[]]
cp (xs:xss)           =  [y:ys | y <- xs, ys <- cp xss]

collapse              :: Matrix [a] -> [Matrix a]
collapse              =  cp . map cp

solve                 :: Grid -> [Grid]
solve                 =  filter valid . collapse . choices

------

prune                 :: Matrix Choices -> Matrix Choices
prune                 =  pruneBy boxs . pruneBy cols . pruneBy rows
                         where pruneBy f = f . map reduce . f
reduce                :: Row Choices -> Row Choices
reduce xss            =  [xs `minus` singles | xs <- xss]
                         where singles = concat (filter single xss)
minus                 :: Choices -> Choices -> Choices
xs `minus` ys         =  if single xs then xs else xs \\ ys

solve2                :: Grid -> [Grid]
solve2                =  filter valid . collapse . prune . choices

-------

solve3                :: Grid -> [Grid]
solve3                =  filter valid . collapse . fix prune . choices
fix                   :: Eq a => (a -> a) -> a -> a
fix f x               =  if x == x' then x else fix f x'
                         where x' = f x
-----------

complete              :: Matrix Choices -> Bool
complete              =  all (all single)

--Similarly, a matrix is "void" if some square contains no choices:

void                  :: Matrix Choices -> Bool
void                  =  any (any null)

--In turn, we use the term "safe" for matrix for which all rows,
--columns and boxes are consistent, in the sense that they do not
--contain more than one occurrence of the same single choice:

safe                  :: Matrix Choices -> Bool
safe cm               =  all consistent (rows cm) &&
                         all consistent (cols cm) &&
                         all consistent (boxs cm)
consistent            :: Row Choices -> Bool
consistent            =  nodups . concat . filter single

--Finally, a matrix is "blocked" if it is void or unsafe:

blocked               :: Matrix Choices -> Bool
blocked m             =  void m || not (safe m)

solve4                :: Grid -> [Grid]
solve4                =  search . prune . choices
search                :: Matrix Choices -> [Grid]
search m
 | blocked m          =  []
 | complete m         =  collapse m
 | otherwise          =  [g | m' <- expand m
                            , g  <- search (prune m')]

--The function expand behaves in the same way as collapse, except that
--it only collapses the first square with more than one choice:

expand                :: Matrix Choices -> [Matrix Choices]
expand m              =
   [rows1 ++ [row1 ++ [c] : row2] ++ rows2 | c <- cs]
   where
      (rows1,row:rows2) = break (any (not . single)) m
      (row1,cs:row2)    = break (not . single) row

solver :: IO ()
solver = do
    putStrLn (unlines (head (solve4 (easy))))
