--
-- do.hs
--

{-
import Prelude hiding (Maybe, Nothing, Just, return, (>>=))

-- parametric option type Maybe definition
-- definicion del tipo (parametrizado) Maybe ('a' extendido con nulos)
data Maybe a = Nothing | Just a
    deriving Show

(>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
(>>=) mz f = case mz of
    Nothing -> Nothing
    Just z -> f z

instance Monad Maybe where
    Nothing  >>= _ = Nothing
    (Just x) >>= f = f x

-- return simpelmente envuelve un valor de tipo 'a' en un Maybe 'a'
return :: a -> Maybe a
return x = Just x
-}

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

-- division segura
savediv :: Z -> Z -> Maybe Z
savediv _ 0 = Nothing -- not so fast! - la clausula del "aguanta marrano" :-)
savediv x y = Just (x `div` y)

-- este eval es el mas legible de todos ... ¿estan de acuerdo?
eval :: Exp -> Maybe Z
eval (Val n) = return n -- Just n works too!
eval (Div x y) = do
    n <- eval x
    d <- eval y
    savediv n d

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
