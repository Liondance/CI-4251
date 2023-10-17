{-
    fibonacci using 4 styles
-}

-- fibo defined with conditional expression and a single parameter match 
fibo_c n = if (n < 2) then n else fibo_c (n-1) + fibo_c (n-2)

-- fibo with lambda using conditional expression and single parameter match 
fibo_l = \n -> if (n < 2) then n else fibo_l (n-1) + fibo_l (n-2)

-- fibo with multiple pattern matching, instead of conditionals
fibo_p 0 = 0
fibo_p 1 = 1
fibo_p n = fibo_p (n-1) + fibo_p (n-2)

-- fibo with guards
fibo_g n | n == 0    = 0
         | n == 1    = 1
         | otherwise = fibo_g (n-1) + fibo_g (n-2)

-- another fibo with guards
fibo_2 n | x < 2     = x
         | otherwise = fibo_2 (n-1) + fibo_2 (n-2)
