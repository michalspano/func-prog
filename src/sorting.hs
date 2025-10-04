-- A home-made implementation of a sorting function on a list
-- of elements of an abitrary type.
import Test.QuickCheck
import Data.List (sort)

-- Find the minimum from a list of values
minEl :: Ord a => [a] -> a
minEl [x]    = x
minEl (x:xs) = min x $ minEl xs

-- Remove first occurrence of a value from a list of values.
removeFst :: Eq a => a -> [a] -> [a]
removeFst _ [] = []
removeFst m b@[x]
  | x == m     = []
  | otherwise  = b
removeFst m (x:xs)
  | x == m     = xs 
  | otherwise  = x : removeFst m xs

-- Sort a list of values
sort' :: Ord a => [a] -> [a]
sort' []     = []
sort' xs = m : sort' (removeFst m xs)
  where
    m = minEl xs

-- Define some QuickCheck tests on @sort'@. We use @sort@ from @Data.List@
-- class to test against our implementation.
prop_sortedInts :: [Int] -> Bool
prop_sortedInts xs = sort xs == ys
  where
    ys = sort' xs

-- Define a property that verifies an empty list is returned when an empty list
-- is passed to @sort'@.
prop_emptyList :: Bool
prop_emptyList = null (sort' [] :: [Int])
