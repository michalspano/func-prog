-- * 'Higher order functions' example with Quicksort
-- where the comparison logic is defined with a predicate
-- function.

-- | The partition method that receives a predicate
-- function where generic type a evaluates to Bool.
part :: (a -> Bool) -> [a] -> ([a], [a]) 
part p [] = ([], [])
part p (x:xs)
  | p x       = (x: ys, zs)
  | otherwise = (ys, x: zs) 
  where
    (ys, zs) = part p xs

-- Advantage: the sortBy method works on any generic
-- type a, for instance: [(a, a)], etc.
sortBy :: (a -> a -> Bool) -> [a] -> [a]
sortBy cmp xs = qsort xs
 where
  qsort []     = []
  qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
   where
    (smaller, larger) = part (\y -> x `cmp` y) xs

-- Example usage:
-- sortBy (\x y -> x > y) $ reverse [1..100]
-- Sorts a List from lowest to highest
