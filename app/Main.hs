module Main 
    where

import Game (solution)

main :: IO () 
main = do
    putStrLn $ concatMap (\x -> toString x  ++ "\n\n") solution        