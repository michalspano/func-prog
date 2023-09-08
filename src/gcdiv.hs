-- Implementation of `gcd` greatest common
-- divisor in Haskell

{--
   GCD (Greatest Common Divisor)
   Example: gcd (9 6) -> 3
   The implementation is based on the Euclidean
   algorithm, namely:
      gcd (x, y) => (y, x mod ) -> ... -> (d, 0).
-}
gcdiv :: Int -> Int -> Int
gcdiv x 0 = x
gcdiv x y = gcdiv y (x `mod` y)
