-- Does d divide n?
divides :: Int -> Int -> Bool
divides d n = rem n d == 0

-- Least divisor d of n, s.t. d >= k.
ld :: Int -> Int -> Int
ld k n | divides k n = k
       | k^2 >   n   = n
       | otherwise   = ld (k+1) n

-- Least divisor d of n, s.t. d >= 2. The least prime factor is 2.
ld' :: Int -> Int
ld' = ld 2

-- Apply `ld` to determine the primality
isPrime :: Int -> Bool
isPrime n | n < 0     = error "Negative value not allowed."
          | n == 1    = False
          | otherwise = ld' n == n

