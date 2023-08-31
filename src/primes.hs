-- List of all primes until n
-- a Haskell implementation

-- determine if y is a factor of x
isFactorOf :: Int -> Int -> Bool
isFactorOf x y = y `mod` x == 0

-- list all factors of n
factors :: Int -> [Int]
factors n = [x | x <- [1 .. n], x `isFactorOf` n]

-- a prime is only divisible by 1 and itself
isPrime :: Int -> Bool
isPrime x = factors x == [1, x]

-- list all primes until n
primes :: Int -> [Int]
primes n = [x | x <- [2 .. n], isPrime x]
