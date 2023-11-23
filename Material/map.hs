{-
  map
-}

{- datos -}

l0 = []

li = [ 14, 23, 13, 22, 12 ]

lz = [ 1, 997 .. 1000 * 1000 * 1000]

lc = [ 'h', 'o', 'l', 'a' ]

ls = [ "This", "is", "how", "we", "do", "it" ]

lb = [ True, False, True, False, True, False ]

{- implementaciones  -}

mapz :: (a -> b) -> [a] -> [b]
mapz f [] = []
mapz f (x:xs) = f x : (mapz f xs)

map' :: (a -> b) -> [a] -> [b]
map' f list = [ f x | x <- list ]

--------


{-

-}

type Map k v = [(k, v)]

sym =
    [
        ( "Alice",  "917-456-2020"),
        ( "Betty",  "732-745-3123"),
        ( "Carl",   "617-888-1234"),
        ( "David",  "415-777-6789")
    ]

apply :: Eq k => Map k v -> k -> Maybe v
apply map key = lookup key map

(?) :: Eq k => Map k v -> k -> Maybe v
-- (?) map key = lookup key map
(?) = apply