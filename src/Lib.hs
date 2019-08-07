module Lib
    ( insertListEvery
    , newLineEvery
    )
    where

newLineEvery :: Int -> String -> String
newLineEvery n cs =
    insertListEvery n cs "\n"


insertListEvery :: Int -> [a] -> [a] -> [a]
insertListEvery n xs insertion =
    let
        f y (count, ys) = 
            let
                ys' = ys ++ [y]
                count' = count + 1
            in
                if mod count' n == 0 then
                    (count', ys' ++ insertion)
                else
                    (count', ys')        
        
        (_, result) = foldr f (0, []) xs
    in
        result            

-- newLineEvery :: Int -> String -> String
-- newLineEvery n x =
--     let
--         f :: Char -> (Int, [Char]) -> (Int, [Char])
--         f c (count, cs) = 
--             let
--                 cs' = cs ++ [c]
--                 count' = count + 1
--             in
--                 if mod count' n == 0 then
--                     (count', cs' ++ "\n")
--                 else
--                     (count', cs')        
        
--         (_, result) = foldr f (0, []) x
--     in
--         result        