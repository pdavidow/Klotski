{-# LANGUAGE InstanceSigs #-}

module Game
    where     

import Data.Function ( (&) )
import Data.List ((\\), find, foldr, sort, sortBy, union, unfoldr)
import Debug.Trace (trace, traceIO)

import Lib (newLineEvery)

type Square = (Int, Int) -- (x, y) zero-based
data Squares2 = Squares2 Square Square deriving (Eq, Show)
data Squares4 = Squares4 Square Square Square Square deriving (Eq, Show)

data Orientation = Horz | Vert deriving (Eq, Ord, Show)

data Tile1 = Tile1 Square deriving (Eq, Ord, Show)   
data Tile2  = Tile2 Orientation Square deriving (Eq, Ord, Show) -- left square for Horiz; top for Vert
--data Tile2 Orientation = Tile2 Square deriving (Eq, Show) -- left square for Horiz; top for Vert
data Tile4 = Tile4 Square deriving (Eq, Ord, Show) -- top-left square

-- type Tile1s = Tile1s Tile1 Tile1 Tile1 Tile1 deriving (Eq, Show)
-- type Tile2s = Tile2s (Tile2 Horz) (Tile2 Vert) (Tile2 Vert) (Tile2 Vert) (Tile2 Vert) deriving (Eq, Show)

data Space1 = Space1 Square deriving (Eq, Show)
data Space2 = Space2 Orientation Square deriving (Eq, Show) -- left square for Horiz; top for Vert

data Spaces 
    = Separate Space1 Space1
    | Combined Space2
        deriving (Eq, Show)    

data Board = Board [Tile1] [Tile2] Tile4 deriving (Ord, Show)   
-- todo https://hackage.haskell.org/package/sorted-list-0.2.1.0/docs/Data-SortedList.html
-- data Board = Board Tile1s Tile2s Tile4 deriving (Eq, Show)   

data MoveTile1 = MoveTile1 Tile1 Tile1 deriving (Eq, Ord, Show)
data MoveTile2 = MoveTile2 Tile2 Tile2 deriving (Eq, Ord, Show)
data MoveTile4 = MoveTile4 Tile4 Tile4 deriving (Eq, Ord, Show)

data MoveTiles = MoveTiles [MoveTile1] [MoveTile2] [MoveTile4] deriving (Show)

data StartState = StartState Board deriving (Eq, Ord, Show)
data MidState = MidState ParentState Board deriving (Eq, Ord, Show)
data EndState = EndState ParentState Board deriving (Eq, Ord, Show)
 
type ParentState = Tagged_State

data Tagged_State
    = Tagged_StartState StartState
    | Tagged_MidState MidState
    | Tagged_EndState EndState
        deriving (Eq, Ord, Show)     
        

instance Eq Board where
    (==) :: Board -> Board -> Bool
    (==) (Board at1s at2s at4) (Board bt1s bt2s bt4) = 
        ( at4 == bt4 ) &&
        ( sort at2s == sort bt2s ) &&
        ( sort at1s == sort bt1s )


instance Eq MoveTiles where
    (==) :: MoveTiles -> MoveTiles -> Bool
    (==) (MoveTiles at1s at2s at4s) (MoveTiles bt1s bt2s bt4s) =
        ( sort at4s == sort bt4s ) &&
        ( sort at2s == sort bt2s ) &&
        ( sort at1s == sort bt1s )    


boardWidth :: Int
boardWidth = 
    4


boardHeight :: Int
boardHeight = 
    5 
    --3         


squares2ToList :: Squares2 -> [Square]
squares2ToList (Squares2 a b) =
    [a, b]
      
    
squares4ToList :: Squares4 -> [Square]
squares4ToList (Squares4 a b c d) =
    [a, b, c, d]


toSquares2 :: Orientation -> Square -> Squares2
toSquares2 o sq1@(i, j) = 
    Squares2 sq1 sq2
        where sq2 = case o of 
                Horz -> (i + 1, j)
                Vert -> (i    , j + 1) 
 

tile2ToSquares :: Tile2 -> Squares2
tile2ToSquares (Tile2 o sq) =
    toSquares2 o sq


spacesToSquares :: Spaces -> Squares2
spacesToSquares (Separate (Space1 a) (Space1 b)) = 
    Squares2 a b    
spacesToSquares (Combined x) = 
    space2ToSquares x


space2ToSquares :: Space2 -> Squares2
space2ToSquares (Space2 o sq) =
    toSquares2 o sq


tile4ToSquares :: Tile4 -> Squares4
tile4ToSquares (Tile4 topLeft@(i, j)) = 
    Squares4 topLeft topRight bottomLeft bottomRight
        where
            topRight = (i + 1, j)
            bottomLeft = (i, j + 1)
            bottomRight = (i + 1, j + 1)     


isMirror :: Board -> Board -> Bool
isMirror (Board ta1s ta2s ta4) (Board tb1s tb2s tb4) = 
    -- horizontal symmetry
    let
        isMirrorTile1 :: (Tile1, Tile1) -> Bool
        isMirrorTile1 ((Tile1 aSq), (Tile1 bSq)) =
            isMirrorSquare (aSq, bSq)    

        isMirrorTile2 :: (Tile2, Tile2) -> Bool
        isMirrorTile2 ((Tile2 ao aSq), (Tile2 bo bSq)) =
            ao == bo &&
                case ao of
                    Horz -> isMirrorHorz (aSq, bSq)
                    Vert -> isMirrorVert (aSq, bSq)  
            
        isMirrorTile4 :: (Tile4, Tile4) -> Bool
        isMirrorTile4 ((Tile4 aSq), (Tile4 bSq)) =
            isMirrorHorz (aSq, bSq)  

        isMirrorTile1s :: ([Tile1], [Tile1]) -> Bool
        isMirrorTile1s (as, bs) =
            all (\a -> any (\b -> isMirrorTile1 (a, b)) bs) as

        isMirrorTile2s :: ([Tile2], [Tile2]) -> Bool
        isMirrorTile2s (as, bs) =
            all (\a -> any (\b -> isMirrorTile2 (a, b)) bs) as

        isMirrorSquare :: (Square, Square) -> Bool
        isMirrorSquare ((ai, aj), (bi, bj)) =  
            (aj == bj) && 
            (ai == boardWidth - bi - 1)    
            
        isMirrorHorz :: (Square, Square) -> Bool
        isMirrorHorz ((ai, aj), (bi, bj)) =   
            (aj == bj) && 
            (ai == boardWidth - bi - 2)    
            
        isMirrorVert :: (Square, Square) -> Bool      
        isMirrorVert x =   
            isMirrorSquare x  
    in
        isMirrorTile4 (ta4, tb4) &&
        isMirrorTile2s (ta2s, tb2s) &&
        isMirrorTile1s (ta1s, tb1s) 


toNonStartState :: ParentState -> Board -> Tagged_State
toNonStartState x b =
    if isSolved b then
        Tagged_EndState $ EndState x b
    else
        Tagged_MidState $ MidState x b


moveTile1 :: Tagged_State -> MoveTile1 -> Tagged_State
moveTile1 x (MoveTile1 tile1 tile1') =
    let
        (Board t1s t2s t4) = toBoard x
        --t1s' = map (\ t -> if t == tile1 then tile1' else t) t1s

        f = (\ t (isDone, ts) -> if not isDone && t == tile1 then (True, tile1' : ts) else (False, t : ts))
        (_, t1s') = foldr f (False, []) t1s

        t1s'' = sort t1s' -- for efficiency
    in
        toNonStartState x $ Board t1s'' t2s t4


moveTile2 :: Tagged_State -> MoveTile2 -> Tagged_State
moveTile2 x (MoveTile2 tile2 tile2') =
    let
        (Board t1s t2s t4) = toBoard x
        --t2s' = map (\ t -> if t == tile2 then tile2' else t) t2s

        f = (\ t (isDone, ts) -> if not isDone && t == tile2 then (True, tile2' : ts) else (False, t : ts))        
        (_, t2s') = foldr f (False, []) t2s

        t2s'' = sort t2s' -- for efficiency
    in
        toNonStartState x $ Board t1s t2s'' t4


moveTile4 :: Tagged_State -> MoveTile4 -> Tagged_State
moveTile4 x (MoveTile4 _ tile4') =
    let
        (Board t1s t2s _) = toBoard x
    in
        toNonStartState x $ Board t1s t2s tile4'


movesOn :: Board -> MoveTiles
movesOn b =
    --case trace ("\nfindSpaces b: " ++ (show $ findSpaces b)) findSpaces b of
    case findSpaces b of
        Separate sa sb -> 
            let
                (MoveTiles a1s a2s a4s) = movesOnSeparateSpace1 b sa
                (MoveTiles b1s b2s b4s) = movesOnSeparateSpace1 b sb
            in
                MoveTiles (sort (a1s ++ b1s)) (sort (a2s ++ b2s)) (sort (a4s ++ b4s))

        Combined s -> 
            movesOnSpace2 b s


movesOnSeparateSpace1 :: Board -> Space1 -> MoveTiles
movesOnSeparateSpace1 b (Space1 sSq@(i, j)) =
    let
        (Board t1s t2s _) = b

        mt1s = foldr 
            (\ source@(Tile1 tSq) acc -> 
                let
                    dest = Tile1 sSq

                    result = maybe [] (\ x -> [MoveTile1 source x]) $
                        if 
                            tSq == (i - 1, j) ||
                            tSq == (i + 1, j) ||
                            tSq == (i, j - 1) ||
                            tSq == (i, j + 1)
                        then
                            Just dest
                        else 
                            Nothing             
                in
                    acc ++ result 
            ) [] t1s 

        mt2s = foldr 
            (\ source@(Tile2 tO _) acc -> 
                let   
                    (Squares2 t1 t2) = tile2ToSquares source
                    
                    result = maybe [] (\ x -> [MoveTile2 source $ Tile2 tO x]) $    
                        case tO of
                            Horz ->
                                if      t2 == (i - 1, j) then Just t2
                                else if t1 == (i + 1, j) then Just sSq
                                else Nothing

                            Vert ->
                                if      t2 == (i, j - 1) then Just t2
                                else if t1 == (i, j + 1) then Just sSq
                                else Nothing     
                in
                    acc ++ result                     
            ) [] t2s 
    in
        MoveTiles mt1s mt2s []


movesOnSpace2 :: Board -> Space2 -> MoveTiles
movesOnSpace2 (Board t1s t2s t4) s@(Space2 sO sSq) =
    let
        (Squares2 s1 s2) = space2ToSquares s
        (s1i, s1j) = s1
        (s2i, s2j) = s2
            
        mt1s = foldr 
            (\ source@(Tile1 tSq) acc -> 
                let
                    result = maybe [] (\ xs -> map (\ x -> MoveTile1 source $ Tile1 x) xs) $     
                        case sO of
                            Horz ->                        
                                if      tSq == (s1i, s1j - 1) || tSq == (s1i, s1j + 1) then Just [s1] -- top, bottom on left
                                else if tSq == (s2i, s2j - 1) || tSq == (s2i, s2j + 1) then Just [s2] -- top, bottom on right
                                else if tSq == (s1i - 1, s1j) || tSq == (s2i + 1, s1j) then Just [s1, s2] -- left, right cap
                                else Nothing -- never the case

                            Vert ->
                                if      tSq == (s1i - 1, s1j) || tSq == (s1i + 1, s1j) then Just [s1] -- left, right on top                                        
                                else if tSq == (s2i - 1, s2j) || tSq == (s2i + 1, s2j) then Just [s2] -- left, right on bottom 
                                else if tSq == (s1i, s1j - 1) || tSq == (s2i, s2j + 1) then Just [s1, s2] -- top, bottom cap                                        
                                else Nothing -- never the case                                        
                in
                    acc ++ result
            ) [] t1s 
                            
        mt2s = foldr 
            (\ source@(Tile2 tO _) acc ->                         
                let   
                    (Squares2 t1 t2) = tile2ToSquares source  

                    result = maybe [] (\ xs -> map (\ x -> MoveTile2 source $ Tile2 tO x) xs) $
                        case sO of
                            Horz ->    
                                case tO of
                                    Horz ->
                                        if      t1 == (s1i, s1j - 1) || t1 == (s1i, s1j + 1) then Just [sSq] -- top, bottom
                                        else if t2 == (s1i - 1, s1j) then Just [(s1i - 1, s1j), s1] -- left
                                        else if t1 == (s2i + 1, s1j) then Just [s2, s1] -- right                                                  
                                        else Nothing

                                    Vert ->
                                        if      t2 == (s1i, s1j - 1) || t2 == (s2i, s2j - 1) then Just [t2] -- top left, top right
                                        else if t1 == (s1i, s1j + 1) then Just [s1] -- bottom left 
                                        else if t1 == (s2i, s2j + 1) then Just [s2] -- bottom right         
                                        else Nothing  

                            Vert ->  
                                case tO of
                                    Horz -> 
                                        if      t2 == (s1i - 1, s1j) || t2 == (s2i - 1, s2j) then Just [t2] -- top left, bottom left      
                                        else if t1 == (s1i + 1, s1j) then Just [s1] -- top right     
                                        else if t1 == (s2i + 1, s2j) then Just [s2] -- bottom right                                             
                                        else Nothing  

                                    Vert -> 
                                        if      t1 == (s1i - 1, s1j) || t1 == (s1i + 1, s1j) then Just [sSq] -- left, right                                                            
                                        else if t2 == (s1i, s1j - 1) then Just [(s1i, s1j - 1), s1] -- top                                                                                                     
                                        else if t1 == (s2i, s2j + 1) then Just [s2, s1]  -- bottom                                                        
                                        else Nothing                                                                                                   
                in
                    acc ++ result                          
            ) [] t2s      
            
        mt4s = foldr 
            (\ source acc ->                         
                let   
                    (Squares4 t_topLeft t_topRight t_bottomLeft _) = tile4ToSquares source  
                                        
                    result =  maybe [] (\ x -> [MoveTile4 source $ Tile4 x]) $                           
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
                    acc ++ result 
            ) [] [t4]                     
    in
        MoveTiles (sort mt1s) (sort mt2s) (sort mt4s)


boardSquares :: [Square]
boardSquares =
    [ (i,j)::Square | i <- [0..(boardWidth-1)], j <- [0..(boardHeight - 1)] ]


toString :: Board -> String
toString b@(Board t1s t2s t4) = 
   let
        t1Char = '1'
        t2Char o = case o of 
            Horz -> 'h' 
            Vert -> 'v'
        t4Char = '4'
        spaceChar = '*'

        t1Pixels = map (\(Tile1 sq) -> (sq, t1Char)) t1s
        t2Pixels = concatMap (\t@(Tile2 o _) -> let char = t2Char o in map (\sq -> (sq, char)) $ squares2ToList $ tile2ToSquares t) t2s
        t4Pixels = map (\sq -> (sq, t4Char)) $ squares4ToList $ tile4ToSquares t4   
        spacePixels = map (\sq -> (sq, spaceChar)) $ squares2ToList $ spacesToSquares $ findSpaces b
    in
        t1Pixels ++ t2Pixels ++ t4Pixels ++ spacePixels
            & sortBy (\((ai, aj), _) ((bi, bj), _) -> compare (bj, bi) (aj, ai))
            & map (\(_, c) -> c)            
            & newLineEvery boardWidth


tileSquares :: Board -> [Square]
tileSquares (Board t1s t2s t4) =
    let
        t1Sqs = map (\(Tile1 sq) -> sq) t1s
        t2Sqs = concatMap (squares2ToList . tile2ToSquares) t2s
        t4Sqs = squares4ToList $ tile4ToSquares t4
    in
        t1Sqs ++ t2Sqs ++ t4Sqs


findSpaces :: Board -> Spaces
findSpaces b =
    let
        sSqs = boardSquares \\ tileSquares b  

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


toBoard :: Tagged_State -> Board
toBoard (Tagged_StartState (StartState x)) = x
toBoard (Tagged_MidState (MidState   _ x)) = x
toBoard (Tagged_EndState (EndState   _ x)) = x


startState :: Tagged_State
startState =
    Tagged_StartState $ StartState startBoard


startBoard :: Board
startBoard =
    let  
        t1s = sort -- for efficiency
            [ Tile1 (1, 3)
            , Tile1 (2, 3)
            , Tile1 (0, 4)
            , Tile1 (3, 4)
            ]  

        t2s = sort -- for efficiency
            [ Tile2 Horz (1, 2)
            , Tile2 Vert (0, 0)
            , Tile2 Vert (3, 0)
            , Tile2 Vert (0, 2)
            , Tile2 Vert (3, 2)
            ]    

        t4 = Tile4 (1, 0)
    in
        Board t1s t2s t4  

    -- let  
    --     t1s = 
    --         [ Tile1 (1, 2)
    --         , Tile1 (2, 2)
    --         ]  

    --     t2s = 
    --         [ Tile2 Vert (0, 0)
    --         , Tile2 Vert (3, 0)
    --         ]    

    --     t4 = Tile4 (1, 0)
    -- in
    --     Board t1s t2s t4  


childStates :: Tagged_State -> [Tagged_State]
childStates x =
    let
        (MoveTiles mt1s mt2s mt4s) = movesOn $ toBoard x   
    in
        if isEndState x then
            []
        else
            map (moveTile1 x) mt1s ++
            map (moveTile2 x) mt2s ++
            map (moveTile4 x) mt4s       
            

childStatesOfInterest :: [Tagged_State] -> Tagged_State -> [Tagged_State]
childStatesOfInterest visited x = 
    let
        f :: Tagged_State -> Bool
        f c = 
            not $ any (\v -> 
                let
                    cb = toBoard c
                    vb = toBoard v
                in
                    (==) cb vb || isMirror cb vb) visited                    
    in
        filter f $ childStates x


isSolved :: Board -> Bool
isSolved (Board _ _ (Tile4 sq)) = 
    sq == (1, 3)
    --sq == (1, 1)


isEndState :: Tagged_State -> Bool
isEndState (Tagged_EndState _) = True
isEndState _ = False


solution :: [Board] 
solution =   
    -- breadth-first search: use queue (better Data.Sequence) to first vist all children before moving to grandchildren
    -- http://aleph.nz/post/search_in_haskell/

    let
        go :: [Tagged_State] -> [Tagged_State] -> [Tagged_State]
        go _ [] = error "solution not reached"
        go visited toBeVisited =
            let
                -- current = trace("current :\n" ++ (toString $ toBoard $ last toBeVisited)) last toBeVisited
                -- current = trace("current :\n" ++ (show $ toBoard $ last toBeVisited)) last toBeVisited
                -- visited' = trace ("\n\nvisited' : " ++ show (visited ++ [current])) visited ++ [current]
                -- q = trace ("\ntoBeVisited : " ++ concatMap ((++) "\n" . toString . toBoard) toBeVisited) toBeVisited
                --toBeVisited' = trace ("\ntoBeVisited' : " ++ concatMap ((++) "\n" . toString . toBoard) (childStatesOfInterest visited current ++ init toBeVisited)) (childStatesOfInterest visited current) ++ init toBeVisited
            
                current = last toBeVisited
                --visited' = visited ++ [current]
                visited' = current : visited
                toBeVisited' = childStatesOfInterest visited current ++ init toBeVisited
            in            
                if isEndState current then
                    visited'
                else
                    go visited' toBeVisited'

        xs = go [] [startState]
        lastVisited = last xs

        f :: Tagged_State -> Maybe (Board, Tagged_State)
        f x = 
            case x of
                Tagged_StartState _ -> Nothing
                Tagged_MidState (MidState p b) -> Just (b, p)
                Tagged_EndState (EndState p b) -> Just (b, p)

        headlessTrail = reverse $ unfoldr f lastVisited
    in
        (toBoard $ head xs) : headlessTrail