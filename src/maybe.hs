-- Using the `Maybe` type in Haskell 
-- Division by zero not permitted.

safeDivide :: Int -> Int -> Maybe Int
safeDivide _ 0 = Nothing
safeDivide x y = Just (x `div` y)

main :: IO ()
main = do
  putStrLn "Enter two numbers:"
  num1 <- readLn
  num2 <- readLn
  case safeDivide num1 num2 of
    Just result -> putStrLn $ "Result: " ++ show result
    Nothing     -> putStrLn "Division by zero or invalid input."
