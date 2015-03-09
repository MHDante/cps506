module Assign3 where
    import Data.Char
    ----------COLOR
    data Color = Black|White
    instance Show Color where
        show Black = "B"
        show White = "W"
    negate :: Color -> Color
    negate Black = White
    negate White = Black
    ----------END COLOR
    
    ----------PIECE 
    data Piece = King Color|Queen Color|Rook Color|Knight Color|Bishop Color|Pawn Color|Empty
    instance Show Piece where
        show (King a) = show a ++ "K "
        show (Queen a) = show a ++ "Q "
        show (Rook a) = show a ++ "R "
        show (Knight a) = show a ++ "Kn"
        show (Bishop a) = show a ++ "B "
        show (Pawn a) = show a ++ "P "
        show Empty = "   "
    ----------END PIECE

    ----------BOARD
    newtype Board = Board [[Piece]]
    select :: Board -> Int -> Int -> Piece
    select (Board b) x y = b!!y!!x
    instance Show Board where
        show (Board x) =  divider ++ showHelper x ++ letters where
            showHelper xs@(h:t) = showHelper t ++ show (length xs) ++ showRow h ++ divider where
                showRow (h2:t2)  = "|" ++ show h2 ++ showRow t2
                showRow [] ="|\n"
            showHelper [] = ""
    ----------END BOARD

    data BoardState a = BoardState Board [Piece] [Piece] Color Special
    
    data Special = Special {canCastle::Bool, enPassant::(Int,Int), inCheck::Bool, inCheckmate::Bool}

    --applyMove ::  BoardState -> (Board -> String -> (b,[c])) -> (b,[c])
    --applyMove _ = 
    parse :: String -> (Piece, String)
    parse (x:y:' ':xs) = (select defaultBoard a n, xs) where
        a = ord x - ord 'a'
        n = ord y - ord '0'
    parse _ = (Empty, "")

    divider :: String
    divider = " " ++ take 33 (cycle "+---") ++ "\n"
    letters :: String
    letters = concat ["   "++[j] | j<-['a'..'h']]

    defaultBoard :: Board
    defaultBoard = Board [
        [Rook White, Knight White, Bishop White, Queen White, King White, Bishop White, Knight White, Rook White]
        ,replicate  8 (Pawn White)
        ,replicate  8 Empty
        ,replicate  8 Empty
        ,replicate  8 Empty
        ,replicate  8 Empty
        ,replicate  8 (Pawn Black)
        ,[Rook Black, Knight Black, Bishop Black, Queen Black, King Black, Bishop Black, Knight Black, Rook Black]]


    main :: IO ()
    main = print defaultBoard --
    --main = print $ parse "a1 xy"
    