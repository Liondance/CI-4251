{-
    Interactive Fibonacci function tester
-}

fibo :: Integer -> Integer
fibo 0 = 0
fibo 1 = 1
fibo n = fibo (n-1) + fibo (n-2)

-- REPL

prompt = "<λ> "

fibl = do
    putStr prompt
    putStrLn "enter n:"
    s <- getLine -- READ
    if s == "."
        then return ()
        else do
            let result = fibo (read s) -- EVAL
            putStrLn (show result) -- PRINT
            fibl -- LOOP

main = fibl
