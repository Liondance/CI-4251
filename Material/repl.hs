{-
    REPL Skeleton
-}

interpret :: String -> String
interpret s =
        let val = length s in
        (show s ++ " is " ++ show val ++ " chars long")

-- REPL

prompt = "<H> "

repl = do
    putStr prompt
    s <- getLine -- READ
    if s == "."
        then return ()
        else do
            let result = interpret s -- EVAL
            putStrLn result -- PRINT
            repl -- LOOP
