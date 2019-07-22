module Main 
    where

import Data.Tree.Game_tree.Negascout (principal_variation_search)
import Game

searchDepth :: Int
searchDepth = 9

main :: IO ()
main = do
    let (states, score) = principal_variation_search (Tagged_StartState startState) searchDepth
    let boardPrints = concatMap (\x -> "\n" ++ (show $ boardFromState x)) states

    putStrLn $ "# moves: " ++ (show $ length states)
    putStrLn $ "score: " ++ show score
    putStrLn "=========================="
    putStrLn boardPrints
