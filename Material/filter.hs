{-
  filter
-}

{- datos -}

l0 = []

li = [ 14, 23, 13, 22, 12 ]

lz = [ 1, 997 .. 1000 * 1000 * 1000]

lc = [ 'h', 'o', 'l', 'a' ]

ls = [ "This", "is", "how", "we", "do", "it" ]

lb = [ True, False, True, False, True, False ]

{- implementaciones -}

-- 1
filter1 :: (a -> Bool) -> [a] -> [a]
filter1 _ [] = []
filter1 pred (x:xs) = (if pred x then x:filter1 pred xs else filter1 pred xs)

-- 2
filter2 :: (a -> Bool) -> [a] -> [a]
filter2 pred []                 = []
filter2 pred (x:xs) | pred x    = x:filter2 pred xs
                    | otherwise =   filter2 pred xs
