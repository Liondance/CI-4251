{-
    Interactive factorial
-}

import System.IO

fact :: Integer -> Integer
fact n = product [1 .. n]

run = do
    putStr "x: "
    s <- getLine
    let x = read s :: Integer
    putStr "y: "
    s <- getLine
    let y = read s :: Integer
    let z = fmap (fact) [x, y]
    putStrLn ("fmap (fact) [" ++ (show x) ++ ", " ++ show y ++ "] = " ++ show z)
    return ()

main = run