module Assign3 where
    import Data.Char
    import Data.Maybe
    ----------COLOR
    data Color = Black|White deriving (Eq)
    instance Show Color where
        show Black = "B"
        show White = "W"
    negate :: Color -> Color
    negate Black = White
    negate White = Black
    ----------END COLOR
    data Rank = King|Queen |Rook |Knight |Bishop |Pawn deriving (Eq)
    instance Show Rank where
        show King =     "K"
        show Queen =    "Q"
        show Rook =     "R"
        show Knight =   "Kn"
        show Bishop =   "B"
        show Pawn =     "P"
    ----------PIECE 
    data Piece = Piece Rank Color|Empty deriving (Eq)
    instance Show Piece where
        show (Piece Knight a) = show a ++ "Kn"
        show (Piece r a) = show a ++ show r ++" "
        show Empty = "   "

    eatOrOccupy :: Board -> Color -> Coord -> Maybe Action
    eatOrOccupy board col crd = eooHelper (select board crd) where
        eooHelper Nothing = Nothing
        eooHelper (Just Empty) = Just (Occupy crd)
        eooHelper (Just (Piece _ tCol)) = if col /= tCol then Just (Eat crd) else Nothing

    takeIfOccupied :: Board -> Color -> Coord -> Maybe Action
    takeIfOccupied board col crd = tioHelper (select board crd) where
        tioHelper Nothing = Nothing
        tioHelper (Just Empty) = Nothing
        tioHelper (Just (Piece _ tCol)) = if col /= tCol then Just (Eat crd) else Nothing

    dirTable :: String -> Coord
    dirTable "N" = (0,1)
    dirTable "S" = (0,-1)
    dirTable "E" = (1,0)
    dirTable "W" = (-1,0)
    dirTable "NE" = (1,1)
    dirTable "NW" = (-1,1)
    dirTable "SE" = (1,-1)
    dirTable "SW" = (-1,-1)
    dirTable _ = (0,0)
    
    type Analyzer = Board-> Coord-> Maybe Action
    getAnalyzer::Piece -> String -> Analyzer
    
    getAnalyzer Empty _ = \_ _->Nothing
    
    getAnalyzer x [d,' '] = getAnalyzer x (d:" 8")
    getAnalyzer x [d1,d2,' '] = getAnalyzer x (d1:d2:" 8")
    getAnalyzer x [d1,d2,d3,' '] = getAnalyzer x [d1,d2,d3]
    getAnalyzer x [d] = getAnalyzer x (d:" 8")
    getAnalyzer x [d1,d2] = getAnalyzer x (d1:d2:" 8")


    getAnalyzer (Piece Pawn Black) ['S',' ',d] = \board (x,y) -> if notElem d ['1'..'8'] then Nothing else
            if d /= '1' && y == 6 && (select board (x,y-1) == Just Empty) && (select board (x,y-2) == Just Empty)
                then Just (Occupy (x,y-2)) else if select board (x,y-1) == Just Empty
                    then Just (Occupy (x,y-1)) else Nothing
    getAnalyzer (Piece Pawn White) ['N',' ',d] = \board (x,y) -> if notElem d ['1'..'8'] then Nothing else
            if d /= '1' && y == 1 && (select board (x,y+1) == Just Empty) && (select board (x,y+2) == Just Empty)
                then Just (Occupy (x,y+2)) else if select board (x,y+1) == Just Empty
                    then Just (Occupy (x,y+1)) else Nothing


    getAnalyzer (Piece Pawn Black) ['S','E',' ',d]
        | d `elem` ['1'..'8'] = \board (x,y) -> takeIfOccupied board Black (x-1,y-1)
        | otherwise = \_ _-> Nothing
    getAnalyzer (Piece Pawn Black) ['S','W',' ',d]
        | d `elem` ['1'..'8'] = \board (x,y) -> takeIfOccupied board Black (x+1,y-1)
        | otherwise = \_ _-> Nothing
    getAnalyzer (Piece Pawn White) ['N','E',' ',d]
        | d `elem` ['1'..'8'] = \board (x,y) -> takeIfOccupied board White (x-1,y+1)
        | otherwise = \_ _-> Nothing
    getAnalyzer (Piece Pawn White) ['N','W',' ',d]
        | d `elem` ['1'..'8'] = \board (x,y) -> takeIfOccupied board White (x+1,y+1)
        | otherwise = \_ _-> Nothing
    
    getAnalyzer (Piece Pawn _) _ = \_ _ -> Nothing
        
    getAnalyzer (Piece Knight c) "NNE" = \board (x,y) -> eatOrOccupy board c (x+1,y+2)
    getAnalyzer (Piece Knight c) "NNW" = \board (x,y) -> eatOrOccupy board c (x-1,y+2)
    getAnalyzer (Piece Knight c) "EEN" = \board (x,y) -> eatOrOccupy board c (x+2,y+1)
    getAnalyzer (Piece Knight c) "EES" = \board (x,y) -> eatOrOccupy board c (x+2,y-1)
    getAnalyzer (Piece Knight c) "SSE" = \board (x,y) -> eatOrOccupy board c (x+1,y-2)
    getAnalyzer (Piece Knight c) "SSW" = \board (x,y) -> eatOrOccupy board c (x-1,y-2)
    getAnalyzer (Piece Knight c) "WWN" = \board (x,y) -> eatOrOccupy board c (x-2,y+1)
    getAnalyzer (Piece Knight c) "WWS" = \board (x,y) -> eatOrOccupy board c (x-2,y-1)
    getAnalyzer (Piece Knight _) _ = \_ _ -> Nothing
    
    getAnalyzer (Piece Rook _) [_,_,' ',_] = \_ _ -> Nothing
    getAnalyzer (Piece Bishop _) [_,' ',_] = \_ _ -> Nothing
    getAnalyzer (Piece King c) [d,' ',_] = getAnalyzer (Piece Queen c) [d,' ','1']
    getAnalyzer (Piece King c) [d1,d2,' ',_] = getAnalyzer (Piece Queen c) [d1,d2,' ','1']
    getAnalyzer (Piece _ c) [dir,' ',l] = let d = dirTable [dir] in
                                            if d == (0,0) then \_ _ -> Nothing else analHelper d (digitToInt l) 1 c
    getAnalyzer (Piece _ c) [d1,d2,' ',l] = let d = dirTable [d1,d2] in
                                            if d == (0,0) then \_ _ -> Nothing else analHelper d (digitToInt l) 1 c
    getAnalyzer _ _ = \_ _ -> Nothing


    analHelper :: Coord -> Int -> Int -> Color -> Board -> Coord -> Maybe Action
    analHelper (dx,dy) limit i c board (x,y) 
        | i > limit = Nothing
        | isNothing (select board coords) = Nothing
        | otherwise = if select board coords /= Just Empty then eatOrOccupy board c coords else
                          let next = analHelper (dx,dy) limit (i+1) c board (x,y) in
                              if isNothing next then eatOrOccupy board c coords else next
        where coords = (x+i*dx, y+i*dy)
    ----------END PIECE

    ----------BOARD
    newtype Board = Board [[Piece]]
    select :: Board -> Coord -> Maybe Piece
    select (Board b) (x,y) 
        | y>(length b - 1) || y<0 || x<0 || x > length (head b) = Nothing
        | otherwise = Just $ b!!y!!x

    divider :: String
    divider = " " ++ take 33 (cycle "+---")
    letters :: String
    letters = concat ["   "++[j] | j<-['a'..'h']]
    
    instance Show BoardState where
        show (BoardState (Board board) b w turn m) =  divider ++ "\n"++showHelper board (1::Int) ++ letters where
            showHelper xs@(h:t) i = showHelper t (i+1) ++ show (length xs) ++ showRow h ++ divider ++ msg i where
                showRow (h2:t2)  = "|" ++ show h2 ++ showRow t2
                showRow [] ="|\n"
            showHelper [] _ = ""
            msg 8 = "  White Captures: " ++ show w ++"\n"
            msg 6 = "  Black Captures: " ++ show b ++"\n"
            msg 4 = "  Current Turn: " ++ show turn ++"\n"
            msg 2 = "  "++m ++"\n"
            msg _ = "\n"
    ----------END BOARD

    data BoardState = BoardState {getBoard::Board
                                 , blackCaptures::[Piece]
                                 , whiteCaptures::[Piece]
                                 , getTurn::Color
                                 , message::String}

    
    type Coord = (Int, Int)
    data Action = Occupy Coord |Eat Coord deriving(Eq)
    defAnalyzer::Analyzer
    defAnalyzer _ _ = Nothing
    
    applyMove ::  BoardState -> String -> BoardState
    applyMove bs@(BoardState board b w turn _) mv
        | pc == Empty = BoardState board b w turn "Could Not Select Piece"
        | let (Piece _ c) = pc in c /= turn = BoardState board b w turn ("Not your Turn!"++ show pc)
        | otherwise = let act = anlz board coord in
                          if isNothing act then BoardState board b w turn "Could Not Perform Move" 
                              else updateState bs (fromJust act) coord
        where (pc, coord, anlz) = fromMaybe (Empty, (0,0), defAnalyzer) (parse board mv)

    updateState :: BoardState -> Action -> Coord -> BoardState
    updateState (BoardState (Board board) b w turn _) (Eat (x1,y1)) (x0,y0)
        |turn == White = BoardState (Board (updateBoard board 0 0)) (b ++ [pc]) w Black (show pc ++ " ate " ++ show opc)
        |turn == Black = BoardState (Board (updateBoard board 0 0)) b (w ++ [pc]) White (show pc ++ " ate " ++ show opc) 
        where 
            pc = fromJust (select (Board board) (x0,y0))
            opc = fromJust (select (Board board) (x1,y1))
            updateBoard [] _ _ = []
            updateBoard (h1:t1) x2 y2 = updateRow h1 x2 y2 : updateBoard t1 x2 (y2+1)
                where
                    updateRow (h2:t2) x3 y3 =  (if x1 == x3 && y1 == y3
                        then pc else if x0 == x3 && y0 == y3 then Empty else h2) : updateRow t2 (x3+1) y3
                    updateRow [] _ _ = []
    updateState (BoardState (Board board) b w turn _) (Occupy (x1,y1)) (x0,y0) =
        BoardState (Board (updateBoard board 0 0)) b w (if turn == White then Black else White) (show pc ++ " was moved to " ++ show (x1, y1)) where 
            pc = fromJust (select (Board board) (x0,y0))
            updateBoard [] _ _ = []
            updateBoard (h1:t1) x2 y2 = updateRow h1 x2 y2 : updateBoard t1 x2 (y2+1)
                where
                    updateRow (h2:t2) x3 y3 =  (if x1 == x3 && y1 == y3
                        then pc else if x0 == x3 && y0 == y3 then Empty else h2) : updateRow t2 (x3+1) y3
                    updateRow [] _ _ = []
    updateState bs _ _ = bs

    parse :: Board -> String -> Maybe (Piece, Coord, Analyzer)
    parse board (a:n:' ':xs)
        | isNothing mPc = Nothing
        | notElem a ['a'..'h'] || notElem n['1'..'8'] = Nothing 
        | otherwise = let pc = fromJust mPc in Just (pc, coord, getAnalyzer pc xs)
        where
            coord = (ord a - ord 'a',8 - (ord n - ord '0'))
            mPc = select board coord
    parse _ _ = Nothing


    defaultBoard :: Board
    defaultBoard = Board [
        [Piece Rook White, Piece Knight White, Piece Bishop White, Piece Queen White, Piece King White, Piece Bishop White, Piece Knight White, Piece Rook White]
        ,replicate 8 (Piece Pawn White)
        ,replicate 8 Empty
        ,replicate 8 Empty
        ,replicate 8 Empty
        ,replicate 8 Empty
        ,replicate 8 (Piece Pawn Black)
        ,[Piece Rook Black, Piece Knight Black, Piece Bishop Black, Piece Queen Black, Piece King Black, Piece Bishop Black, Piece Knight Black, Piece Rook Black]]

    main :: IO ()
    --main = print defaultBoard --
    --main = print $ parse "a1 xy"

    main = do 
        let board = BoardState defaultBoard [] [] White ""
        main2 board

    main2 :: BoardState -> IO ()
    main2 bs = do
        print bs
        input <- getLine
        let b2 = applyMove bs input
        main2 b2
    