
import System.IO
import Control.Concurrent

cls :: IO ()
cls = putStr "\ESC[2J"

run 0 = return ()
run n = do
    cls
    let l = [0 .. n-1]
    putStrLn (show l)
    threadDelay 1000000
    run (n - 1)
