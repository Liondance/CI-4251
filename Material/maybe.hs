--
-- maybe.hs
--

import Prelude hiding (Maybe, Nothing, Just, return, (>>=))

-- super-simple lenguaje (solo division) con expresiones enteras
type Z = Integer

data Exp =  Val Z | Div Exp Exp
    deriving Show

-- sample data for testing - ejemplos para pruebas

suite :: [Exp]
suite =
    [
        -- (42 / 5) / 2
        Div (Div (Val 42) (Val 5)) (Val 2),
        -- (42 / 5) / 0
        Div (Div (Val 42) (Val 5)) (Val 0),
        -- (42 / 0) / 2
        Div (Div (Val 42) (Val 0)) (Val 2),
        -- (0 / 5) / 2
        Div (Div (Val  0) (Val 5)) (Val 2),
        -- 0 / 0
        Div (Val 0) (Val 0)
    ]

-- parametric option type Maybe definition
-- definicion del tipo (parametrizado) Maybe ('a' extendido con nulos)
data Maybe a = Nothing | Just a
    deriving Show

-- return simplemente envuelve un valor de tipo 'a' en un Maybe 'a'
return :: a -> Maybe a
return x = Just x

-- Para probar las versiones ...
-- deben quitarle el comentario a una y solo una a la vez

{-
-- version 0: naive: throws an exception when the denominator is 0
-- version 0: ingenua: arroja una exception cuando el denominador es 0

-- esta es la funcion que evalua, pero regresa un Z
-- por lo tanto no es compatible con el arnes de pruebas
-- es por esto que le damos un nombre distinto
naive :: Exp -> Z
naive (Val n) = n
naive (Div n d) = ((naive n) `div` (naive d))

-- 'naive' es la funcion que evalua las expresiones
-- pero tiene tipo Exp -> Z y las proximas versiones tienen tipo Exp -> Maybe Z
-- 'eval' envuelve ("wraps") a 'naive' para ser consistente con las otras versiones
-- de esta manera el arnes de pruebas funciona igual para todas
eval :: Exp -> Maybe Z
eval x = return (naive x) -- Just (naive x) tambien sirve
-}

savediv :: Z -> Z -> Maybe Z
savediv _ 0 = Nothing -- not so fast! - la clausula del "aguanta marrano" :-)
savediv x y = Just (x `div` y)

-- version 1: works but this coding style gets cumbersome quickly
-- version 1: funciona, pero este estilo de programacion se vuelve engorroso rapidamente

{-
eval :: Exp -> Maybe Z
eval (Val n) = return n
eval (Div x y) =
    case eval x of
        Nothing -> Nothing
        Just n -> case eval y of
            Nothing -> Nothing
            Just d -> savediv n d
-}

{-
-- version 2: abstracting the Nothing-Just pattern with a function named 'bind'
-- version 2: abstrayendo el patron Nothing-Just en una funcion llamada 'bind'

-- mz: think of it as a z value (where z <- Z) wrapped by a Maybe (but could also be Nothing!)
--  f: extremely important - f is applied to the z value to continue the computation
bind :: Maybe a -> (a -> Maybe b) -> Maybe b
bind mz f = case mz of
    Nothing -> Nothing
    Just z -> f z

-- this eval version is shorter 
-- it handles the Nothing case in an elegant way, delegating it to 'bind'
-- the continuation function 'f' (there are two here) is expressed with a lambda
-- we can think of 'bind' as binding 'z' to the parameter of the lambda
-- (negative side) this is still hard to read, because of the nested lambdas
-- indenting the two 'bind' calls at the same level improves things a bit
eval :: Exp -> Maybe Z
eval (Val n) = return n -- Just n
eval (Div x y) =
    bind (eval x) (\n ->
        bind (eval y) (\d -> savediv n d))
-}

{-
-- version 3 - uses the infix operator: '>>='
-- version 3 - usa el operador infijo '>>='

-- GREAT idea: defining an infix operator for bind! (notation "sugar")
-- GRAN idea: definir un operator infijo para bind! (azucar sintactico)
(>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
mz >>= f = case mz of
    Nothing -> Nothing
    Just z -> f z

eval :: Exp -> Maybe Z
eval (Val n) = Just n
eval (Div x y) =
    eval x >>= (\n ->
    eval y >>= (\d ->
    savediv n d))
-}

--
-- test
--

-- run one test suite - ejemplo: run factorial
test :: [Exp] -> IO ()
test [] = putStrLn "."
test (x:xs) = do
    putStr (show x)
    putStr (" -> ")
    let result = eval x
    case result of
        Nothing -> putStrLn "Nothing"
        Just z -> putStrLn (show z)
    test xs
