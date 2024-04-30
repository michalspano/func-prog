-- This Haskell source code contains some @QuickCheck@
-- testing properties of the @Data.List@ type, namely
-- the @sort@ function.

import Data.List (sort)

-- Check that the length remains the same.

prop_length :: Ord a => [a] -> Bool
prop_length xs = length xs == length (sort xs)

-- Check that the first el. of the sorted @List@ is
-- the minimum.

prop_fst_min :: Ord a => [a] -> Bool
prop_fst_min xs = head (sort xs) == minimum xs

-- Check that a list is sorted in an increasing manner.
prop_increasing :: Ord a => [a] -> Bool
-- prop_increasing xs = all (\(x, y) -> x <= y) xs'
prop_increasing xs = all (uncurry (<=)) xs'
  where
    xs' = zip xs (tail xs)
