--
-- Monte Carlo
--

-- module MonteCarlo where

import System.IO
import System.Random

-- Histogram descriptor

-- HistDesc = HD (lb, ub, nb)
-- lb :: Double: lower bound
-- ub :: Double: upper bound
-- nb :: Int: number of buckets
newtype HistDesc = HD (Double, Double, Int)
    deriving Show

-- lower bound
lb :: HistDesc -> Double
lb (HD (lbx, _, _)) = lbx

-- upper bound
ub :: HistDesc -> Double
ub (HD (_, ubx, _)) = ubx

-- number of buckets
nb :: HistDesc -> Int
nb (HD (_, _, nbx)) = nbx

newtype Histogram = H (HistDesc, [Int])
    deriving Show

histogram :: Double -> Double -> Int -> Histogram
histogram lb ub nb = H (HD (lb, ub, nb), fvec (nb + 2))

desc :: Histogram -> HistDesc
desc (H (desc, _)) = desc

freq :: Histogram -> [Int]
freq (H (_, fvec)) = fvec

fvec :: Int -> [Int]
fvec 0 = []
fvec n = 0:fvec (n-1)

add :: Histogram -> Double -> Histogram
add (H (desc, oldf)) x = H (desc, newf)
    where
        i = index desc x
        newf = (take i oldf) ++ ( (oldf !! i) + 1: drop (i + 1) oldf)

-- index: find bucket
-- index :: HistDesc -> Double -> Int
index :: HistDesc -> Double -> Int
index (HD (lb, ub, nb)) x   | x  < lb = 0
                            | ub <= x = nb + 1
                            | otherwise = 1 + floor (fromIntegral(nb) * ((x - lb) / (ub - lb)))

u :: () -> IO Double            
u () = randomIO

bm :: (Double, Double) -> (Double, Double)
bm (u1, u2) = (r * cos a, r * sin a)
    where
        r = sqrt(-2 * log u1)
        a = (2 * pi * u2)

mc :: Histogram -> Int -> IO Histogram
mc hist 0 = return hist
mc hist n = do 
    x <- u ()
    -- putStrLn (show x)
    do
        let h = add hist x
        -- putStrLn (show h)
        mc h (n - 1)

-- Test

h = histogram (0.0) (1.0) (10)

run hist = do
    putStr "N evaluaciones: "
    hFlush stdout
    nstr <- getLine
    let n = (read nstr) :: Int in do
        result <- mc hist n
        putStrLn (show result)

main = run h
