-- The FizzBuzz problem in Haskell

main :: IO()
main = mapM_ (putStrLn . fizzbuzz) [1..50]

-- Use the concatenating approach to build the string
fizzbuzz :: Int -> String
fizzbuzz n
    | n `mod` 15  == 0 = "FizzBuzz"
    | n `mod` 3   == 0 = "Fizz"
    | n `mod` 5   == 0 = "Buzz"
    | otherwise   = show n

