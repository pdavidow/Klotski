module Main 
    where

import Game 

main :: IO () 
main = do
    putStrLn $ concatMap (\x -> toString x  ++ "\n\n") solution        