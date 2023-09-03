-- `pow` n k implementation in Haskell.
-- Two different implementations are given
-- and they are tested on a set of 'unit tests'

-- A generic approach to solving n^k;
-- that is, multiply the value of nk times.
power :: Integer -> Integer -> Integer
power n k 
  | k < 0 = error "power: negative argument"
power n 0 = 1
power n k = n * power n (k-1)

-- If k is even, then the formula is:
-- (n * n)^(k/2). Otherwise, we call the function
-- recursively, so that k becomes even, namely:
-- n^(k-1).
power2 :: Integer -> Integer -> Integer
power2 n 0 = 1 -- base case
power2 n k
  | k < 0     = error "power2: negative k"
  | even k    = power2 (n * n) (k `div` 2)
  | otherwise = n * power n (k - 1)

comparePower2 :: Integer -> Integer -> Bool
comparePower2 n k = power n k == power2 n k

-- Test the function `power2` on a set of 
-- 'unit tests'.
testAll = and [comparePower2 n k | n <- [-10..20], k <- [0..20]]
