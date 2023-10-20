-- Quicksort

li = [ 42, 67, 0, 6, 18, 18, 39, 32, 16, 8]

ls = [ "Ian", "Ernesto", "Carlos", "Alicia", "Gladys", "Kim", "Betty", "Henry", "David", "Francesca", "Jacobo"]

-- quicksort in one line
qsort1 [] = []
qsort1 (x:xs) = qsort1 [a | a <- xs, a <= x] ++ [x] ++ qsort1 [b | b <- xs, x <  b]

-- quicksort using let
qsortl [] = []
qsortl (x:xs) = let
                    menores = qsortl [a | a <- xs, a <= x]
                    mayores = qsortl [b | b <- xs, x <  b]
                in
                    menores ++ [x] ++ mayores

-- quicksort using where
qsortw [] = []
qsortw (x:xs) = menores ++ [x] ++ mayores
                where
                    menores = qsortw [a | a <- xs, a <= x]
                    mayores = qsortw [b | b <- xs, x <  b]
