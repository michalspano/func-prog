-- * Recursive data type Diagram (Haskell implementation)

data Diagram 
  = Question String Diagram Diagram
  | Action String Diagram
  | Done
  deriving (Show, Eq)

{-
A simple example; visaulisation:

   isSunny 
     (Q)
  /       \
park      work
(A)       (A)
 |         |
Done      Done
-}

isSunny, park, work :: Diagram
isSunny = Question "Is it sunny outside?" park work
park    = Action "Go to the park! And write some Haskell code!" Done
work    = Action "Write some Haskell code!" Done
