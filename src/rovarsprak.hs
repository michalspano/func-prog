-- The Robber Language (Sve: Rövarspråket)
-- Rules: every consonant is doubled and an "o"
-- is inserted between, such as "s" -> "sos"

import Data.Char (toLower, isAlpha)

toRobber :: String -> String
toRobber cs = concat [applyRule c | c <- cs]

applyRule :: Char -> String
applyRule c
  | isConsonant c = [c] ++ "o" ++ [toLower c]
  | otherwise     = [c]

isConsonant :: Char -> Bool
isConsonant c = c `notElem` "aeiouyåöä" && isAlpha c

translate :: IO ()
translate = do
  putStrLn "Welcome to the Robber Language translator!\n"
  putStr   "Type a sentence:\n> "
  s <- getLine
  putStrLn "In robber language:"
  putStrLn (toRobber s)

