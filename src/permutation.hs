-- This is a Haskell implementation of a function
-- that checks if a List if a permutation of another. 

isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation []     [] = True
isPermutation [] (y:ys) = False
isPermutation (x:xs) ys = x `elem` ys && isPermutation xs (removeOnce x ys) 

-- Remove a chosen element at its first occurence
removeOnce :: Eq a => a -> [a] -> [a] 
removeOnce _ [] = []
removeOnce x (y:ys)
  | x == y    = ys
  | otherwise = y: removeOnce x ys

