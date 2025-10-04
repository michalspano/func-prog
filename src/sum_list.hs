sumList :: Num a => [a] -> a
sumList [x]   = x
sumList (x:y) = (+) x $ sumList y

sumList' :: Num a => [a] -> a
sumList' xs = foldr (+) 0 xs
