-- fibonacci: natural recursion
-- this implementation has time complexity O(2^n)
fibonr :: Int -> Int
fibonr 0 = 0
fibonr 1 = 1
fibonr n = fibonr (n - 1) + fibonr (n - 2)

-- fibonacci: tail recursion (a.k.a. "recursive iteration")
-- this implementation has time complexity O(n)
fibotr :: Int -> Int
fibotr 0 = 0
fibotr 1 = 1
fibotr n = fibotr' (n - 2) 1 0
            where
                fibotr' 0 nm1 nm2 = nm1 + nm2
                fibotr' k nm1 nm2 = fibotr' (k - 1) (nm1 + nm2) nm1

-- prebuilt tests
test n = (fibonr n, fibotr n)

r0  = test  0
r1  = test  1
r2  = test  2
r8  = test  8
r10 = test 10
r20 = test 20
r24 = test 24
r28 = test 28
r32 = test 32
