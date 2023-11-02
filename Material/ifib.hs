{-
    Interactive Fibonacci
-}

import System.IO
import System.Random

-- recursive definition

fibo :: Int -> Int
fibo 0 = 0
fibo 1 = 1
fibo n = fibo (n-1) + fibo (n-2)

interpret :: String -> Int
interpret s = fibo (read s)

-- REPL

prompt = "<H> "

ifib = do
    putStr (prompt ++ "n: ")
    hFlush stdout
    s <- getLine -- READ
    if s == "."
        then return ()
        else do
            let result = interpret s -- EVAL
            putStrLn $ ("fibo " ++ s ++ " ==> " ++ (show result)) -- PRINT
            ifib -- LOOP

main = ifib

u :: IO Double
u = randomIO
