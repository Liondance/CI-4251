{-
  folds
-}

{- datos -}

l0 = []

li = [ 14, 23, 13, 22, 12 ]

lz = [ 1, 997 .. 1000 * 1000 * 1000]

lc = [ 'h', 'o', 'l', 'a' ]

ls = [ "This", "is", "how", "we", "do", "it" ]

lb = [ True, False, True, False, True, False ]


{-

  fold: operacion binaria -> lista -> resultado

  Pero ... ¿ Cual fold estamos implementando ?

  x0 @ (x1 @ .. xn)

  (x0 @ x1) @ .. xn

  fold generaliza (+) para implementar 'sum' y (*) para implementar 'product'.

-}

{-
  @ folded from the right

  fr [] = v
  fr (x:xs) = x @ fr xs

  [ 1, 2, 3, 4 ]

  1 : ( 2 : (3 : (4 : [])) )

  1 + 2 + 3 + 4 = 1 + (2 + (3 + (4 + 0))) = 1 + (2 + (3 + 4)) = 1 + (2 + 7) = 1 + 9 = 10
-}

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f v [] = v
foldr' f v (x:xs) = f x (foldr' f v xs)

{- foldr op2 init list -}


{-
  @ folded from the left

  f v [] = v
  f v (x:xs) =
-}

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' _ v [] = v
foldl' f v (x:xs) = foldl' f (f v x) xs
