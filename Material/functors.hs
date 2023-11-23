--
-- functors.hs
--

import Prelude hiding (map, fmap, Functor, Maybe, Nothing, Just)

ivec :: [Int]
ivec = [ 0, 1, 2, 3, 10, 20 ]

nvec :: [Integer]
nvec = [ 0, 1, 2, 3, 10, 20 ]

xvec :: [Double]
xvec = [ 0, 1, 2, 3, 10, 20 ]

----

inc :: Num a => [a] -> [a]
inc [] = []
inc (n:ns) = n + 1 : inc ns

square :: Num a => [a] -> [a]
square [] = []
square (x:xs) = x^2 : square xs

ipower2 :: Integral a => [a] -> [a]
ipower2 [] = []
ipower2 (x:xs) = 2^x : ipower2 xs

fpower2 :: Floating a => [a] -> [a]
fpower2 [] = []
fpower2 (x:xs) = 2**x : fpower2 xs

-- map

map :: (a -> b) -> [a] -> [b]
map f [] = []
map f (x:xs) = f x : map f xs

-- 
inc' = map (+1)

square' = map (^2)

ipower2' = map (2^)

fpower2' = map (2**)

--------

-- Functor!

-- Functor: clase de tipos que permiten una "mapping function"

-- clase Functor definida en el preludio
class Functor f where
    fmap :: (a -> b) -> f a -> f b

instance Functor [] where
    -- fmap :: (a -> b) -> [a] -> [b]
    fmap = map

data Maybe a = Nothing | Just a

instance Functor Maybe where
    -- fmap :: (a -> b) -> Maybe a -> Maybe b
    fmap _ Nothing = Nothing
    fmap f (Just x) = Just (f x)

data Arbol a = Hoja a | Nodo (Arbol a) (Arbol a)
    deriving Show

instance Functor Arbol where
    -- fmap :: (a -> b) -> Arbol a -> Arbol b
    fmap f (Hoja a) = Hoja (f a)
    fmap f (Nodo lhs rhs) = Nodo (fmap f lhs) (fmap f rhs)

arbol = Nodo (Nodo (Hoja 6) (Hoja 7)) (Nodo (Hoja 42) (Hoja 67))

-- evaluar
-- fmap (+1) arbol
-- fmap (^2) arbol

{-
    ============
    Functor Laws
    ============

    fmap id         = id
    fmap (g . h)    = fmap g . fmap h
-}

-- evaluar
-- fmap ((+1) . (^2)) [ 2, 3 ]
-- (fmap (+1) . fmap (^2)) [ 2, 3 ]

{-
    esta definicion no cumple las leyes

    instance Functor [] where
        -- fmap :: (a -> b) -> [a] ->  [b]
        fmap f []       = []
        fmap f (x:xs)   = fmap f (xs) ++ [f x]
-}
