{-# LANGUAGE InstanceSigs #-}

module Game
    where     

import Data.Function ( (&) )
import Data.List (union)
import Data.Tree.Game_tree.Game_tree

type Square = (Int, Int) -- (x, y) zero-based

data Orientation = Horz | Vert deriving (Eq, Show)

data Tile1 = Tile1 Square deriving (Eq, Show)   
data Tile2  = Tile2 Orientation Square deriving (Eq, Show) -- left square for Horiz; top for Vert
data Tile4 = Tile4 Square deriving (Eq, Show) -- top-left square

data Space1 = Space1 Square deriving (Eq, Show)
data Space2 = Space2 Orientation Square deriving (Eq, Show) -- left square for Horiz; top for Vert

data Spaces 
    = Separate Space1 Space1
    | Combined Space2
        deriving (Eq, Show)    

data Board = Board [Tile1] [Tile2] Tile4 deriving (Eq, Show)   

type ToIgnore1 = [(Tile1, Tile1)]
type ToIgnore2 = [(Tile2, Tile2)]
type ToIgnore4 = [(Tile4, Tile4)]

data MoveTile1 = MoveTile1 Tile1 Tile1 ToIgnore1 deriving (Eq, Show)
data MoveTile2 = MoveTile2 Tile2 Tile2 ToIgnore2 deriving (Eq, Show)
data MoveTile4 = MoveTile4 Tile4 Tile4 ToIgnore4 deriving (Eq, Show)

data MoveTiles = MoveTiles [MoveTile1] [MoveTile2] [MoveTile4] deriving (Eq, Show)

data PriorMove
    = PriorMove_MoveTile1 MoveTile1
    | PriorMove_MoveTile2 MoveTile2
    | PriorMove_MoveTile4 MoveTile4
        deriving (Eq, Show)

data StartState = StartState MoveTiles Board deriving (Eq, Show)
data MidState = MidState PriorMove MoveTiles Board deriving (Eq, Show)
data EndState = EndState PriorMove Board deriving (Eq, Show)
 
data Tagged_State
    = Tagged_StartState StartState
    | Tagged_MidState MidState
    | Tagged_EndState EndState
        deriving (Eq, Show)

-------------------

instance Game_tree Tagged_State 
    where

    is_terminal :: Tagged_State -> Bool
    is_terminal (Tagged_StartState _) = False
    is_terminal (Tagged_MidState _) = False
    is_terminal (Tagged_EndState _) = True


    node_value :: Tagged_State -> Int
    node_value (Tagged_StartState _) = 0
    node_value (Tagged_MidState x) = 
        if isPriorMoveTile4 x then 
            90
        else if isPriorMoveTile2 x then 
            50
        else 
            0
    node_value (Tagged_EndState _) = 100


    children :: Tagged_State -> [Tagged_State]
    children x =
        applyMoves x

    
------------------   

squares2 :: Orientation -> Square -> (Square, Square)
squares2 o sq1@(i, j) = 
    (sq1, sq2)   
        where sq2 = case o of 
                Horz -> (i + 1, j)
                Vert -> (i, j + 1) 
 

tSquares2 :: Tile2 -> (Square, Square)
tSquares2 (Tile2 o sq) =
    squares2 o sq


sSquares2 :: Space2 -> (Square, Square)
sSquares2 (Space2 o sq) =
    squares2 o sq


tSquares4 :: Tile4 -> (Square, Square, Square, Square)
tSquares4 (Tile4 topLeft@(i, j)) = 
    (topLeft, topRight, bottomLeft, bottomRight)
        where
            topRight = (i + 1, j)
            bottomLeft = (i, j + 1)
            bottomRight = (i + 1, j + 1)       
        
        
boardWidth :: Int
boardWidth = 4


boardHeight :: Int
boardHeight = 5     


moveTile1 :: Board -> MoveTile1 -> Tagged_State
moveTile1 (Board t1s t2s t4) m@(MoveTile1 tile1 tile1' toIgnore1) =
    let
        t1s' = map (\ t -> if t == tile1 then tile1' else t) t1s
        board = Board t1s' t2s t4

        priorMove = PriorMove_MoveTile1 m

        (MoveTiles mt1s mt2s mt4s) = tilesToMove board
        f = \ (MoveTile1 t t' _) -> not $ any (\ x -> x == (t, t')) toIgnore1
        mt1s' = filter f mt1s
        moveTiles = MoveTiles mt1s' mt2s mt4s
        --moveTiles = MoveTiles mt1s mt2s mt4s
    in
        if isSolved board then
            Tagged_EndState $ EndState priorMove board
        else
            Tagged_MidState $ MidState priorMove moveTiles board


moveTile2 :: Board -> MoveTile2 -> Tagged_State
moveTile2 (Board t1s t2s t4) m@(MoveTile2 tile2 tile2' toIgnore2) =
    let
        t2s' = map (\ t -> if t == tile2 then tile2' else t) t2s
        board = Board t1s t2s' t4

        priorMove = PriorMove_MoveTile2 m

        (MoveTiles mt1s mt2s mt4s) = tilesToMove board
        f = \ (MoveTile2 t t' _) -> not $ any (\ x -> x == (t, t')) toIgnore2
        mt2s' = filter f mt2s
        moveTiles = MoveTiles mt1s mt2s' mt4s
        --moveTiles = MoveTiles mt1s mt2s mt4s
    in
        if isSolved board then
            Tagged_EndState $ EndState priorMove board
        else
            Tagged_MidState $ MidState priorMove moveTiles board


moveTile4 :: Board -> MoveTile4 -> Tagged_State
moveTile4 (Board t1s t2s t4) m@(MoveTile4 tile4 tile4' toIgnore4) =
    let
        board = Board t1s t2s tile4'

        priorMove = PriorMove_MoveTile4 m

        (MoveTiles mt1s mt2s mt4s) = tilesToMove board
        f = \ (MoveTile4 t t' _) -> not $ any (\ x -> x == (t, t')) toIgnore4
        mt4s' = filter f mt4s
        moveTiles = MoveTiles mt1s mt2s mt4s'
        --moveTiles = MoveTiles mt1s mt2s mt4s
    in
        if isSolved board then
            Tagged_EndState $ EndState priorMove board
        else
            Tagged_MidState $ MidState priorMove moveTiles board


tilesToMove :: Board -> MoveTiles
tilesToMove b =
    case findSpaces b of
        Separate sa sb -> 
            let
                (MoveTiles a1s a2s a4s) = tilesToMoveForSeparateSpace1 b sa
                (MoveTiles b1s b2s b4s) = tilesToMoveForSeparateSpace1 b sb
            in
                MoveTiles (a1s ++ b1s) (a2s ++ b2s) (a4s ++ b4s)

        Combined s -> 
            tilesToMoveForSpace2 b s


-- tilesToMove :: Board -> MoveTiles
-- tilesToMove b@(Board t1s t2s t4 spaces) =
--     case spaces of
--         Separate sa1 sb1 -> tilesToMoveForSeparateSpace1 t1s t2s (sa1, sb1)
--         Combined s2 -> tilesToMoveForSpace2 t1s t2s t4 s2


tilesToMoveForSeparateSpace1 :: Board -> Space1 -> MoveTiles
tilesToMoveForSeparateSpace1 b (Space1 sSq@(i, j)) =
    let
        (Board t1s t2s _) = b

        mt1s = foldr 
            ( \ source@(Tile1 tSq) acc -> 
                let
                    dest = Tile1 sSq

                    result = 
                        if 
                            tSq == (i - 1, j) ||
                            tSq == (i + 1, j) ||
                            tSq == (i, j - 1) ||
                            tSq == (i, j + 1)
                        then
                            [ MoveTile1 source dest [(dest, source)] ]
                        else 
                            []                    
                in
                    acc ++ result 
            ) [] t1s 

        mt2s = foldr 
            ( \ source@(Tile2 tO tSq) acc -> 
                let   
                    (t1, t2) = tSquares2 source
                    
                    mbDestSq =    
                        case tO of
                            Horz ->
                                if      t2 == (i - 1, j) then Just t2
                                else if t1 == (i + 1, j) then Just sSq
                                else Nothing

                            Vert ->
                                if      t2 == (i, j - 1) then Just t2
                                else if t1 == (i, j + 1) then Just sSq
                                else Nothing     
                                
                    result = maybe [] (\ destSq -> let dest = Tile2 tO destSq in [MoveTile2 source dest [(dest, source)]]) mbDestSq
                in
                    acc ++ result                     
            ) [] t2s 
    in
        MoveTiles mt1s mt2s []


tilesToMoveForSpace2 :: Board -> Space2 -> MoveTiles
tilesToMoveForSpace2 b@(Board t1s t2s t4) s@(Space2 sO sSq) =
    let
        (s1, s2) = sSquares2 s
        (s1i, s1j) = s1
        (s2i, s2j) = s2
            
        mt1s = foldr 
            ( \ source@(Tile1 tSq) acc -> 
                let
                    result =                         
                        let
                            mbDestSq =     
                                case sO of
                                    Horz ->                        
                                        if      tSq == (s1i, s1j - 1) || tSq == (s1i, s1j + 1) || tSq == (s1i - 1, s1j) then Just s1 -- surround left 
                                        else if tSq == (s2i, s2j - 1) || tSq == (s2i, s2j + 1) || tSq == (s2i + 1, s2j) then Just s2 -- surround right 
                                        else Nothing -- never the case

                                    Vert ->
                                        if      tSq == (s1i - 1, s1j) || tSq == (s1i + 1, s1j) || tSq == (s1i, s1j - 1) then Just s1 -- surround top 
                                        else if tSq == (s2i - 1, s2j) || tSq == (s2i + 1, s2j) || tSq == (s2i, s2j + 1) then Just s2 -- surround bottom 
                                        else Nothing -- never the case                                        
                        in
                            maybe [] (\ destSq -> let dest = Tile1 destSq in [MoveTile1 source dest [(dest, source)]]) mbDestSq
                in
                    acc ++ result
            ) [] t1s 
                            
        mt2s = foldr 
            ( \ source@(Tile2 tO tSq) acc ->                         
                let   
                    (t1, t2) = tSquares2 source         
                    
                    result =  
                        case sO of
                            Horz ->    
                                case tO of
                                    Horz ->
                                        if t1 == (s1i, s1j - 1) || t1 == (s1i, s1j + 1) then -- top, bottom
                                            let 
                                                dest = Tile2 tO sSq 
                                            in 
                                                [MoveTile2 source dest [(dest, source)]]

                                        else if t2 == (s1i - 1, s1j) then -- left
                                            let
                                                dest1 = Tile2 tO (s1i - 1, s1j)
                                                dest2 = Tile2 tO s1
                                            in
                                                [ MoveTile2 source dest1 [(dest1, source), (dest1, dest2)]
                                                , MoveTile2 source dest2 [(dest2, source), (dest2, dest1)]
                                                ]

                                        else if t1 == (s2i + 1, s1j) then -- right
                                            let
                                                dest1 = Tile2 tO s2
                                                dest2 = Tile2 tO s1
                                            in
                                                [ MoveTile2 source dest1 [(dest1, source), (dest1, dest2)]
                                                , MoveTile2 source dest2 [(dest2, source), (dest2, dest1)]
                                                ]                                                    
                                        else 
                                            []

                                    Vert ->
                                        let
                                            mbDestSq = 
                                                if      t2 == (s1i, s1j - 1) || t2 == (s2i, s2j - 1) then Just t2 -- top left, top right
                                                else if t1 == (s1i, s1j + 1) then Just s1 -- bottom left 
                                                else if t1 == (s2i, s2j + 1) then Just s2 -- bottom right         
                                                else Nothing  
                                        in
                                            maybe [] (\ destSq -> let dest = Tile2 tO destSq in [MoveTile2 source dest [(dest, source)]]) mbDestSq

                            Vert ->  
                                case tO of
                                    Horz -> 
                                        let
                                            mbDestSq = 
                                                if      t2 == (s1i - 1, s1j) || t2 == (s2i - 1, s2j)then Just t2 -- top left, bottom left      
                                                else if t1 == (s1i + 1, s1j) then Just s1 -- top right     
                                                else if t1 == (s2i + 1, s2j) then Just s2 -- bottom right                                             
                                                else Nothing  
                                        in
                                            maybe [] (\ destSq -> let dest = Tile2 tO destSq in [MoveTile2 source dest [(dest, source)]]) mbDestSq


                                    Vert -> 
                                        if t1 == (s1i - 1, s1j) || t1 == (s1i + 1, s1j) then -- left, right
                                            let 
                                                dest = Tile2 tO sSq 
                                            in 
                                                [MoveTile2 source dest [(dest, source)]]       
                                                
                                        else if t2 == (s1i, s1j - 1) then -- top
                                            let
                                                dest1 = Tile2 tO (s1i, s1j - 1)
                                                dest2 = Tile2 tO s1
                                            in
                                                [ MoveTile2 source dest1 [(dest1, source), (dest1, dest2)]
                                                , MoveTile2 source dest2 [(dest2, source), (dest2, dest1)]
                                                ]      
                                                
                                                
                                        else if t1 == (s2i, s2j - 1) then -- bottom
                                            let
                                                dest1 = Tile2 tO s2
                                                dest2 = Tile2 tO s1
                                            in
                                                [ MoveTile2 source dest1 [(dest1, source), (dest1, dest2)]
                                                , MoveTile2 source dest2 [(dest2, source), (dest2, dest1)]
                                                ]     
                                                
                                        else 
                                            []                                                
                in
                    acc ++ result                          
            ) [] t2s      
            
        mt4s = foldr 
            ( \ source@(Tile4 tSq) acc ->                         
                let   
                    (t_topLeft, t_topRight, t_bottomLeft, t_bottomRight) = tSquares4 source  
                                        
                    result =  
                        let
                            mbDestSq =                             
                                case sO of
                                    Horz ->   
                                        if      t_topLeft == (s1i, s1j + 1) then Just s1 -- top  
                                        else if t_bottomLeft == (s1i, s1j - 1) then Just t_bottomLeft -- bottom                                                                               
                                        else Nothing  

                                    Vert -> 
                                        if      t_topLeft == (s1i + 1, s1j) then Just s1 -- left 
                                        else if t_topRight == (s1i - 1, s1j) then Just t_topRight -- right          
                                        else Nothing                                                                                                        
                        in
                            maybe [] (\ destSq -> let dest = Tile4 destSq in [MoveTile4 source dest [(dest, source)]]) mbDestSq                                
                in
                    acc ++ result 
            ) [] [t4]                     
    in
        MoveTiles mt1s mt2s mt4s


emptySquares :: [Square]
emptySquares =
    [ (i,j)::Square | i <- [0..(boardWidth-1)], j <- [0..(boardHeight - 1)] ]


toString :: Board -> String
toString b = undefined
    -- let 
    --     sepV = ":"
    --     sepH = "."
    --     sepSpanH = concat $ repeat sepH
    -- in
    --     "................." -- ++

--  .................
--  : 2 : 4   4 : 2 :
--  :   :       :   :
--  : 2 : 4   4 : 2 :
--  .................
--  : 2 : 2   2 : 2 :
--  :   :.......:   :
--  : 2 : 1 : 1 : 2 :
--  :...............:
--  : 1 : o : o : 1 :
--  .................


--  .................
--  :   :       :   :
--  : 2 :   4   : 2 :
--  :   :       :   :
--  .................
--  :   :   2   :   :
--  : 2 :.......: 2 :
--  :   : 1 : 1 :   :
--  :...............:
--  : 1 : o : o : 1 :
--  .................


tileSquares :: Board -> [Square]
tileSquares (Board t1s t2s t4) =
    let
        t1Sqs = map (\(Tile1 sq) -> sq) t1s
        t2Sqs = concatMap (\t -> let (a, b)       = tSquares2 t in [a, b]) t2s
        t4Sqs = concatMap (\t -> let (a, b, c, d) = tSquares4 t in [a, b, c, d]) [t4]
    in
        t1Sqs ++ t2Sqs ++ t4Sqs


findSpaces :: Board -> Spaces
findSpaces b =
    let
        tSqs = tileSquares b
        sSqs = filter (\sSq -> not $ any (\tSq -> sSq == tSq) tSqs) emptySquares

        s0 = sSqs !! 0
        s1 = sSqs !! 1

        (s0i, s0j) = s0
        (s1i, s1j) = s1

        separate = Separate (Space1 s0) (Space1 s1)

        result =
            if s0j == s1j then
                if s1i == s0i + 1 then
                    Combined $ Space2 Horz s0
                else if s1i == s0i - 1 then
                    Combined $ Space2 Horz s1
                else
                    separate
            else if s0i == s1i then
                if s1j == s0j + 1 then
                    Combined $ Space2 Vert s0
                else if s1j == s0j - 1 then
                    Combined $ Space2 Vert s1
                else
                    separate
            else
                separate
    in
        if length sSqs /= 2 then 
            error "must be exactly 2"
        else
            result


startBoard :: Board
startBoard =    
    let
        t1s = 
            [ Tile1 (1, 3)
            , Tile1 (2, 3)
            , Tile1 (0, 4)
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
    in
        Board t1s t2s t4


isPriorMoveTile2 :: MidState -> Bool
isPriorMoveTile2 (MidState priorMove _ _) =    
    case priorMove of  
        PriorMove_MoveTile2 _ -> True
        _ -> False


isPriorMoveTile4 :: MidState -> Bool
isPriorMoveTile4 (MidState priorMove _ _) =    
    case priorMove of  
        PriorMove_MoveTile4 _ -> True
        _ -> False


boardFromState :: Tagged_State -> Board
boardFromState (Tagged_StartState (StartState _ x)) = x
boardFromState (Tagged_MidState (MidState   _ _ x)) = x
boardFromState (Tagged_EndState (EndState     _ x)) = x


startState :: StartState
startState =
    StartState moveTiles board
        where 
            board = startBoard
            moveTiles = tilesToMove board


applyMoves :: Tagged_State -> [Tagged_State]
applyMoves taggedState =
    let
        f = \ (MoveTiles mt1s mt2s mt4s) board ->
            map (moveTile1 board) mt1s ++
            map (moveTile2 board) mt2s ++
            map (moveTile4 board) mt4s 
    in
        case taggedState of
            Tagged_StartState (StartState moveTiles board) -> f moveTiles board
            Tagged_MidState (MidState _   moveTiles board) -> f moveTiles board
            Tagged_EndState _                              -> []


isSolved :: Board -> Bool
isSolved (Board _ _ t4) = 
    tSquares4 t4 == ((1,3), (2,3), (1,4), (2,4))