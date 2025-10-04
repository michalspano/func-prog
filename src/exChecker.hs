{-
   A helper program to determine whether the conversion from recursive to
   explicit definitions is mathematically correct.
-}

import Test.QuickCheck

-- 4) Recursive definition
l :: Int -> Int
l 0 = 2
l 1 = 1
l n = l (n - 1) + l (n - 2)

-- 4) Explicit definition
-- Note: using `floor` may cause some floating point imprecisions. It us used
-- to prevent mismatched types on @comparator@.
l' :: Int -> Int
l' n = floor $ a + b
  where
    a = ((1 + sqrt 5.0) / 2)^n
    b = ((1 - sqrt 5.0) / 2)^n

-- 5) Recursive definition
a :: Int -> Int
a 0 = 1
a 1 = 6
a n = 4 * a (n-1) - 4 * a (n-2)

-- 5) Explicit definition
a' :: Int -> Int
a' n = (1 + 2*n) * 2^n

-- 6) Recursive definition
b :: Int -> Int
b 0 = 4
b 1 = 3
b n = b (n-1) + 2 * b (n-2) + 2*n

-- 6) Explicit definition
b' :: Int -> Int
b' n = 3 * (2^(n+1) + (-1)^n) - n - 5

-- Compare `n` outputs of `f` against `g`. This comparator is used to verify
-- whether the explicit definition holds given the recursive definition.
comparator :: Int -> (Int -> Int) -> (Int -> Int) -> Bool
comparator n f g = all (uncurry (==)) (zip xs ys)
  where
    xs = map f [0..n]
    ys = map g [0..n]

-- Similar as @comparator@, but only shows you the pair-wise comparisons.
trace :: Int -> (Int -> Int) -> (Int -> Int) -> [(Int, Int)]
trace n f g = zip xs ys
  where
    xs = map f [0..n]
    ys = map g [0..n]

-- Using QuickCheck
-- Limit the value of n to [0..100] to prevent stack overflow.

prop_verifySmallRange :: (Int -> Int) -> (Int -> Int) -> Bool
prop_verifySmallRange f g = xs == ys
  where
    xs = [f n | n <- [0..30]]
    ys = [g n | n <- [0..30]]
