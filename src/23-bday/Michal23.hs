module Michal23
    ( partyTime -- * run me
    , trivia    -- * 19:00 - 20:30 . Campus
    , padel     -- * 21:00 - 22:00 . Padel Arena, Frihamnen
    , drinks    -- *      >= 22:00 . City/Järntorget
    ) where

import Puzzle (sol)
import Secret (mayHaveFreeDrink,format)

partyTime = do
    putStrLn "Welcome to Michal's party."
    mapM_ (putStrLn . uncurry format) $ zip [1..] [trivia,padel,drinks]
    ctx <- readFile "in.txt"
    let res = toInteger $ sol ctx
    if mayHaveFreeDrink res then
        putStrLn "You can enjoy a free drink."
    else
        putStrLn "Too bad."
        
trivia = "Start: 19:00 - 20:30 . location: Campus"
padel  = "Start: 21:00 - 22:00 . location: Padel Arena, Frihamnen"
drinks = "Start:      >= 22:00 . location: City/Järntorget"
