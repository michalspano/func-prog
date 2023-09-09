-- Function in Haskell to list all possible casts of 2 dice
-- where sum of the values is n.

dice :: Int -> [(Int, Int)]
dice n
  | n < 2 || n > 12 = error "dice: invalid argument"
dice n = [(x, y) | x <- [1..6], y <- [1..6], x + y == n]
