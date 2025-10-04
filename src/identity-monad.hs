-- Outline a simple identity Monad

mbind :: Monad m => Num a => a -> m a
mbind = return
