
-- fact32 uses 'Int' type: 32 bits precision
fact32 :: Int -> Int
fact32 n = product [1 .. n]

-- factAP uses 'Integer' type: arbitrary precision
factAP :: Integer -> Integer
factAP n = product [1 .. n]

{- overflow test -}

test :: Int -> (Int, Integer)
test n = (fact32 n, factAP (toInteger n))

f20 = test 20
f21 = test 21
