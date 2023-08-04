-- The Fibonacci sequence in Haskell 

fib :: Int -> Int
fib 0 = 0 -- base case
fib 1 = 1 -- base case (1)
fib n = fib (n-1) + fib (n-2) -- recursive case

main :: IO ()
main = do
    putStr "Enter a number: "
    n <- getLine
    let x = read n :: Int
    putStrLn ("The " ++ n ++ "th Fibonacci number is " ++ show (fib x))
