{-
  factorial
-}

fact32 :: Int -> Int
fact32 n = product [1 .. n]


{-
  factorial using the natural induction definition
  note how this leads to a tail recursive implementation
  that is not always the case: contrast with fibonacci
-}

fact32nr :: Int -> Int
fact32nr 1 = 1
fact32nr n = n * fact32nr (n - 1)


{-
  same implementation using an explicit lambda expression
-}

fact32ld :: Int -> Int
fact32ld = \n -> if n == 1 then 1 else n * fact32ld (n - 1)
