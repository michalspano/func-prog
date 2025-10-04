incr :: Integral a => [a] -> [a]
incr = map (`mod` 2) 
