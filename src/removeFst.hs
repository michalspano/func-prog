-- Remove first occurrence of a value from a list of values.
removeFst :: Eq a => a -> [a] -> [a]
removeFst _ [] = []
removeFst m a@[x]
  | x == m     = []
  | otherwise  = a
removeFst m (x:xs)
  | x == m     = xs 
  | otherwise  = x : removeFst m xs

