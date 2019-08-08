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

    , testGroup "moveTile1" $
        let
            t1s = sort
                [ Tile1 (2, 3)
                , Tile1 (0, 4)                    
                , Tile1 (1, 4)
                , Tile1 (3, 4)
                ]  

            t1s' = sort
                [ Tile1 (2, 3)
                , Tile1 (0, 4)                    
                , Tile1 (1, 3)
                , Tile1 (3, 4)
                ]  

            t1s'' = sort
                [ Tile1 (1, 3)
                , Tile1 (0, 4)                    
                , Tile1 (1, 4)
                , Tile1 (3, 4)
                ]  

            t1s''' = sort
                [ Tile1 (2, 3)
                , Tile1 (0, 4)                    
                , Tile1 (1, 4)
                , Tile1 (2, 4)
                ]  

            t1s'''' = sort
                [ Tile1 (2, 3)
                , Tile1 (0, 4)                    
                , Tile1 (2, 4)
                , Tile1 (3, 4)
                ]  

            t1s''''' = sort
                [ Tile1 (2, 4)
                , Tile1 (0, 4)                    
                , Tile1 (1, 4)
                , Tile1 (3, 4)
                ] 

            t2s = sort
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
        in    
            [ testCase "moveTile1 a" $
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

    , testGroup "movesOnSeparateSpace1" $
        let
            t1s = sort
                [ Tile1 (2, 3)
                , Tile1 (0, 4)                    
                , Tile1 (1, 4)
                , Tile1 (3, 4)
                ]  

            t1s' = sort
                [ Tile1 (1, 3)
                , Tile1 (1, 4)                    
                , Tile1 (2, 4)
                , Tile1 (3, 4)
                ]  

            t2s = sort
                [ Tile2 Horz (1, 2)
                , Tile2 Vert (0, 0)
                , Tile2 Vert (3, 0)
                , Tile2 Vert (0, 2)
                , Tile2 Vert (3, 2)
                ]    

            t4 = Tile4 (1, 0)

            b = Board t1s t2s t4
            b' = Board t1s' t2s t4

            mt1s = sort
                [ MoveTile1 (Tile1 (1, 4)) (Tile1 (1, 3))
                , MoveTile1 (Tile1 (2, 3)) (Tile1 (1, 3))   
                , MoveTile1 (Tile1 (3, 4)) (Tile1 (2, 4))                                        
                , MoveTile1 (Tile1 (1, 4)) (Tile1 (2, 4))                         
                , MoveTile1 (Tile1 (2, 3)) (Tile1 (2, 4))
                ]  
                
            mt1s' = sort
                [ MoveTile1 (Tile1 (1, 3)) (Tile1 (2, 3))
                , MoveTile1 (Tile1 (1, 4)) (Tile1 (0, 4))   
                , MoveTile1 (Tile1 (2, 4)) (Tile1 (2, 3))                                        
                ]      
                
            mt2s' = 
                [ MoveTile2 (Tile2 Vert (0, 2)) (Tile2 Vert (0, 3))                                      
                ]                   
        in    
            [ testCase "movesOnSeparateSpace1 a" $ 
                movesOn b @?= MoveTiles mt1s [] []  

            , testCase "movesOnSeparateSpace1 b" $ 
                movesOn b' @?= MoveTiles mt1s' mt2s' []                  
            ]    

    , testGroup "movesOnSpace2" $
        let
            t1s = sort
                [ Tile1 (0, 4)                    
                , Tile1 (1, 4)
                , Tile1 (2, 4)
                , Tile1 (3, 4)
                ]  

            t1s' = sort
                [ Tile1 (2, 3)
                , Tile1 (0, 4)                    
                , Tile1 (2, 4)
                , Tile1 (3, 4)
                ]  

            t1s_thl_shr = sort
                [ Tile1 (1, 3)
                , Tile1 (2, 3)                    
                , Tile1 (1, 4)
                , Tile1 (2, 4)
                ]      
                
            t1s_thr_shl = t1s_thl_shr

            t1s_tv_sv_lr = sort
                [ Tile1 (0, 2)
                , Tile1 (1, 2)                    
                , Tile1 (3, 3)
                , Tile1 (3, 4)
                ]  

            t2s_thl_shr = sort
                [ Tile2 Horz (0, 2)
                , Tile2 Vert (0, 0)
                , Tile2 Vert (3, 0)
                , Tile2 Vert (0, 3)
                , Tile2 Vert (3, 3)
                ]  

            t2s_thr_shl = sort
                [ Tile2 Horz (2, 2)
                , Tile2 Vert (0, 0)
                , Tile2 Vert (3, 0)
                , Tile2 Vert (0, 3)
                , Tile2 Vert (3, 3)
                ]  

            t2s_tht_shb = sort
                [ Tile2 Horz (1, 2)
                , Tile2 Vert (0, 0)
                , Tile2 Vert (3, 0)
                , Tile2 Vert (0, 2)
                , Tile2 Vert (3, 2)
                ]    

            t2s_thb_sht = sort
                [ Tile2 Horz (1, 3)
                , Tile2 Vert (0, 0)
                , Tile2 Vert (3, 0)
                , Tile2 Vert (0, 2)
                , Tile2 Vert (3, 2)
                ]    

            t2s_tvt_svb = sort
                [ Tile2 Horz (1, 2)
                , Tile2 Vert (0, 0)
                , Tile2 Vert (3, 0)
                , Tile2 Vert (3, 2)
                , Tile2 Vert (1, 3)                
                ] 

            t2s_tvb_svt = sort
                [ Tile2 Horz (1, 2)
                , Tile2 Vert (0, 2)
                , Tile2 Vert (3, 0)
                , Tile2 Vert (3, 2)
                , Tile2 Vert (1, 3)                
                ] 

            t2s_tv_sv_lr = sort
                [ Tile2 Horz (2, 2)
                , Tile2 Vert (0, 0)
                , Tile2 Vert (3, 0)
                , Tile2 Vert (0, 3)
                , Tile2 Vert (2, 3)                
                ] 

            t4 = Tile4 (1, 0)

            b_tht_shb = Board t1s t2s_tht_shb t4
            b_thb_sht = Board t1s t2s_thb_sht t4
            b_tvt_svb = Board t1s' t2s_tvt_svb t4
            b_tvb_svt = Board t1s' t2s_tvb_svt t4
            b_thl_shr = Board t1s_thl_shr t2s_thl_shr t4
            b_thr_shl = Board t1s_thr_shl t2s_thr_shl t4
            b_tv_sv_lr = Board t1s_tv_sv_lr t2s_tv_sv_lr t4

            mt1s = sort
                [ MoveTile1 (Tile1 (1, 3)) (Tile1 (1, 4))   
                , MoveTile1 (Tile1 (0, 4)) (Tile1 (1, 4))   
                , MoveTile1 (Tile1 (0, 4)) (Tile1 (2, 4))  
                , MoveTile1 (Tile1 (2, 3)) (Tile1 (2, 4))      
                , MoveTile1 (Tile1 (3, 4)) (Tile1 (2, 4))   
                , MoveTile1 (Tile1 (3, 4)) (Tile1 (1, 4))                                      
                ]     
                
            mt1s_tht_shb = sort
                [ MoveTile1 (Tile1 (2, 4)) (Tile1 (2, 3))     
                , MoveTile1 (Tile1 (1, 4)) (Tile1 (1, 3))                                    
                ]  

            mt1s_tvt_svb = sort
                [ MoveTile1 (Tile1 (0, 4)) (Tile1 (0, 3))     
                , MoveTile1 (Tile1 (0, 4)) (Tile1 (0, 2))                                    
                ]  

            mt1s_tv_sv_lr = sort
                [ MoveTile1 (Tile1 (1, 2)) (Tile1 (1, 3))     
                , MoveTile1 (Tile1 (1, 2)) (Tile1 (1, 4))                                    
                ]  

            mt1s_thl_shr = 
                [ MoveTile1 (Tile1 (2, 3)) (Tile1 (2, 2))                                        
                ]  

            mt1s_thr_shl = 
                [ MoveTile1 (Tile1 (1, 3)) (Tile1 (1, 2))                                        
                ]  

            mt2s_thl_shr = sort
                [ MoveTile2 (Tile2 Vert (3, 0)) (Tile2 Vert (3, 1))   
                , MoveTile2 (Tile2 Vert (3, 3)) (Tile2 Vert (3, 2)) 
                , MoveTile2 (Tile2 Horz (0, 2)) (Tile2 Horz (1, 2))
                , MoveTile2 (Tile2 Horz (0, 2)) (Tile2 Horz (2, 2))                                                 
                ]  

            mt2s_thr_shl = sort
                [ MoveTile2 (Tile2 Vert (0, 0)) (Tile2 Vert (0, 1))   
                , MoveTile2 (Tile2 Vert (0, 3)) (Tile2 Vert (0, 2)) 
                , MoveTile2 (Tile2 Horz (2, 2)) (Tile2 Horz (1, 2))
                , MoveTile2 (Tile2 Horz (2, 2)) (Tile2 Horz (0, 2))                                                 
                ]  

            mt2s_tht_shb = 
                [ MoveTile2 (Tile2 Horz (1, 2)) (Tile2 Horz (1, 3))                                      
                ]  
                
            mt2s_thb_sht =
                [ MoveTile2 (Tile2 Horz (1,3)) (Tile2 Horz (1,2))
                ]

            mt2s_tvt_svb = sort
                [ MoveTile2 (Tile2 Vert (0, 0)) (Tile2 Vert (0, 1))   
                , MoveTile2 (Tile2 Vert (0, 0)) (Tile2 Vert (0, 2)) 
                , MoveTile2 (Tile2 Horz (1, 2)) (Tile2 Horz (0, 2))                                              
                ]  
                
            mt2s_tvb_svt = sort
                [ MoveTile2 (Tile2 Vert (0, 2)) (Tile2 Vert (0, 1))   
                , MoveTile2 (Tile2 Vert (0, 2)) (Tile2 Vert (0, 0))                                             
                ]         
                
            mt2s_tv_sv_lr = sort
                [ MoveTile2 (Tile2 Vert (0, 3)) (Tile2 Vert (1, 3))   
                , MoveTile2 (Tile2 Vert (2, 3)) (Tile2 Vert (1, 3))                                             
                ] 

            mt4s_thb_sht = 
                [ MoveTile4 (Tile4 (1, 0)) (Tile4 (1, 1))
                ]

            mt4s_tvb_svt = 
                [ MoveTile4 (Tile4 (1, 0)) (Tile4 (0, 0))
                ]
        in    
            [ testCase "movesOnSpace2 t1s only" $
                movesOn startBoard @?= MoveTiles mt1s [] []  
                
            , testCase "movesOnSpace2 tile-horiz-top space-horiz-bottom" $
                movesOn b_tht_shb @?= MoveTiles mt1s_tht_shb mt2s_tht_shb []  
                
            , testCase "movesOnSpace2 tile-horiz-bottom space-horiz-top" $
                movesOn b_thb_sht @?= MoveTiles [] mt2s_thb_sht mt4s_thb_sht                 
                
            , testCase "movesOnSpace2 tile-horiz-left space-horiz-right" $
                movesOn b_thl_shr @?= MoveTiles mt1s_thl_shr mt2s_thl_shr []    
                
            , testCase "movesOnSpace2 tile-horiz-right space-horiz-left" $
                movesOn b_thr_shl @?= MoveTiles mt1s_thr_shl mt2s_thr_shl [] 

            , testCase "movesOnSpace2 tile-vert space-vert left-right" $
                movesOn b_tv_sv_lr @?= MoveTiles mt1s_tv_sv_lr mt2s_tv_sv_lr []  

            , testCase "movesOnSpace2 tile-vert-top space-vert-bottom" $
                movesOn b_tvt_svb @?= MoveTiles mt1s_tvt_svb mt2s_tvt_svb []    
                
            , testCase "movesOnSpace2 tile-vert-bottom space-vert-top" $
                movesOn b_tvb_svt @?= MoveTiles [] mt2s_tvb_svt mt4s_tvb_svt                 
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

    