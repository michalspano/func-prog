-- Take a string, say a1a2a3 and produce: `a1a2a2a3a3a3`.
-- That is, to make the string 'blow up', kind off.
blowup :: String -> String
blowup s = blowup' s 1

-- A helper function used with @blowup@.
blowup' :: String -> Int -> String
blowup' [] _ = []
blowup' xs k = term ++ blowup' xs' (k+1)
  where
    xs'  = tail xs
    term = replicate k (head xs)
