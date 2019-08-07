-- http://documentup.com/feuerbach/tasty
 
import Test.Tasty  
import Test.Tasty.HUnit

import Data.List (sort) 

import Game 

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests] 


unitTests = testGroup "Unit tests" $
    [ testGroup "isMirror" $
        let
            t1s = 
                [ Tile1 (1, 3)
                , Tile1 (2, 3)
                , Tile1 (1, 4)
                , Tile1 (3, 4)
                ]  

            t2s = 
                [ Tile2 Horz (0, 2)
                , Tile2 Vert (0, 0)
                , Tile2 Vert (3, 0)
                , Tile2 Vert (0, 3)
                , Tile2 Vert (3, 2)
                ]    
    
            t4 = Tile4 (1, 0)

            t1s' = 
                [ Tile1 (1, 3)
                , Tile1 (2, 3)
                , Tile1 (0, 4)
                , Tile1 (2, 4)
                ]  
                
            t2s' = 
                [ Tile2 Horz (2, 2)
                , Tile2 Vert (0, 0)
                , Tile2 Vert (3, 0)
                , Tile2 Vert (0, 2)
                , Tile2 Vert (3, 3)
                ]    
    
            t4' = Tile4 (1, 0)

            b = Board t1s t2s t4  
            b' = Board t1s' t2s' t4'  
        in
            [ testCase "yes on symmetrical" $ 
                (isMirror startBoard startBoard) @?= True   

            , testCase "yes mirror" $ 
                (isMirror b b') @?= True                                                                         
            ] 

    , testGroup "toString" $
        [ testCase "startBoard" $ 
            (toString startBoard) @?= "v44v\nv44v\nvhhv\nv11v\n1**1\n"  
        ]

    , testGroup "movesOn" $
        let
            t1s = 
                [ Tile1 (2, 3)
                , Tile1 (0, 4)                    
                , Tile1 (1, 4)
                , Tile1 (3, 4)
                ]  

            t1s' = 
                [ Tile1 (2, 3)
                , Tile1 (0, 4)                    
                , Tile1 (1, 3)
                , Tile1 (3, 4)
                ]  

            t1s'' = 
                [ Tile1 (1, 3)
                , Tile1 (0, 4)                    
                , Tile1 (1, 4)
                , Tile1 (3, 4)
                ]  

            t1s''' = 
                [ Tile1 (2, 3)
                , Tile1 (0, 4)                    
                , Tile1 (1, 4)
                , Tile1 (2, 4)
                ]  

            t1s'''' = 
                [ Tile1 (2, 3)
                , Tile1 (0, 4)                    
                , Tile1 (2, 4)
                , Tile1 (3, 4)
                ]  

            t1s''''' = 
                [ Tile1 (2, 4)
                , Tile1 (0, 4)                    
                , Tile1 (1, 4)
                , Tile1 (3, 4)
                ] 

            t2s = 
                [ Tile2 Horz (1, 2)
                , Tile2 Vert (0, 0)
                , Tile2 Vert (3, 0)
                , Tile2 Vert (0, 2)
                , Tile2 Vert (3, 2)
                ]    

            t4 = Tile4 (1, 0)

            b = Board t1s t2s t4
            s = startState
            s' = Tagged_MidState $ MidState s b

            mt1s = 
                [ MoveTile1 (Tile1 (1, 4)) (Tile1 (1, 3))
                , MoveTile1 (Tile1 (2, 3)) (Tile1 (1, 3))   
                , MoveTile1 (Tile1 (3, 4)) (Tile1 (2, 4))                                        
                , MoveTile1 (Tile1 (1, 4)) (Tile1 (2, 4))                         
                , MoveTile1 (Tile1 (2, 3)) (Tile1 (2, 4))
                ]    

            mt1s' = sort
                [ MoveTile1 (Tile1 (3, 4)) (Tile1 (1, 4))   
                , MoveTile1 (Tile1 (3, 4)) (Tile1 (2, 4))   
                , MoveTile1 (Tile1 (0, 4)) (Tile1 (1, 4))
                , MoveTile1 (Tile1 (0, 4)) (Tile1 (2, 4))
                , MoveTile1 (Tile1 (2, 3)) (Tile1 (2, 4))  
                , MoveTile1 (Tile1 (1, 3)) (Tile1 (1, 4))              
                ]                  
        in    
            [ testCase "movesOnSeparateSpace1" $ 
                movesOn b @?= MoveTiles mt1s [] []  
                
            , testCase "movesOnSpace2" $
                movesOn startBoard @?= MoveTiles mt1s' [] []  

            , testCase "moveTile1 a" $
                moveTile1 s' (MoveTile1 (Tile1 (1, 4)) (Tile1 (1, 3))) @?= (Tagged_MidState $ MidState s' (Board t1s' t2s t4))

            , testCase "moveTile1 b" $
                moveTile1 s' (MoveTile1 (Tile1 (2, 3)) (Tile1 (1, 3))) @?= (Tagged_MidState $ MidState s' (Board t1s'' t2s t4))                
            
            , testCase "moveTile1 c" $
                moveTile1 s' (MoveTile1 (Tile1 (3, 4)) (Tile1 (2, 4))) @?= (Tagged_MidState $ MidState s' (Board t1s''' t2s t4))                

            , testCase "moveTile1 d" $
                moveTile1 s' (MoveTile1 (Tile1 (1, 4)) (Tile1 (2, 4))) @?= (Tagged_MidState $ MidState s' (Board t1s'''' t2s t4))                

            , testCase "moveTile1 e" $
                moveTile1 s' (MoveTile1 (Tile1 (2, 3)) (Tile1 (2, 4))) @?= (Tagged_MidState $ MidState s' (Board t1s''''' t2s t4))                         
            ]    

    , testGroup "childStates" $
        let
            s = startState
            t2s = sort [Tile2 Horz (1,2), Tile2 Vert (0,0), Tile2 Vert (3,0), Tile2 Vert (0,2), Tile2 Vert (3,2)]
            t4 = Tile4 (1,0)
        in
            [ testCase "StartState" $ 
                sort (childStates s) @?= sort
                    [ Tagged_MidState (MidState s (Board (sort [Tile1 (1,3), Tile1 (2,3), Tile1 (0,4), Tile1 (1,4)]) t2s t4))   
                    , Tagged_MidState (MidState s (Board (sort [Tile1 (1,3), Tile1 (2,3), Tile1 (0,4), Tile1 (2,4)]) t2s t4))    
                    , Tagged_MidState (MidState s (Board (sort [Tile1 (1,3), Tile1 (2,3), Tile1 (1,4), Tile1 (3,4)]) t2s t4))                                           
                    , Tagged_MidState (MidState s (Board (sort [Tile1 (1,3), Tile1 (2,3), Tile1 (2,4), Tile1 (3,4)]) t2s t4))
                    , Tagged_MidState (MidState s (Board (sort [Tile1 (1,3), Tile1 (2,4), Tile1 (0,4), Tile1 (3,4)]) t2s t4))
                    , Tagged_MidState (MidState s (Board (sort [Tile1 (1,4), Tile1 (2,3), Tile1 (0,4), Tile1 (3,4)]) t2s t4))                  
                    ]                    
            ]

    , testGroup "Eq Board" $
        let
            b1 = Board [Tile1 (0,1), Tile1 (0,2), Tile1 (0,3), Tile1 (0,4)] [Tile2 Horz (0,1), Tile2 Vert (0,2), Tile2 Vert (0,3)] $ Tile4 (1,0)
            b2 = Board [Tile1 (0,4), Tile1 (0,3), Tile1 (0,2), Tile1 (0,1)] [Tile2 Vert (0,3), Tile2 Horz (0,1), Tile2 Vert (0,2)] $ Tile4 (1,0)
        in
            [ testCase "True" $
                (b1 == b2) @?= True

            , testCase "False" $
                (b1 == startBoard) @?= False
            ]
    ]

    