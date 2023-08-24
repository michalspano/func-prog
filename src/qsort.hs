-- Implementation of the quicksort algorithm
-- in Haskell. Acquired from Graham Hutton:
-- https://www.youtube.com/watch?v=rIprO6zoujM.

qsort :: (Ord a) => [a] -> [a]
qsort [] = []
qsort (x : xs) = qsort ys ++ [x] ++ qsort zs
  where
    ys = [a | a <- xs, a <= x]
    zs = [b | b <- xs, b > x]
