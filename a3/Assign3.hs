module Assign3 where

    data Color = Black|White

    instance Show Color where
        show Black = "B"
        show White = "W"

    data Piece = King Color|Queen Color|Rook Color|Knight Color|Bishop Color|Pawn Color|Empty

    instance Show Piece where
        show (King a) = (show a) ++ "K "
        show (Queen a) = (show a) ++ "Q "
        show (Rook a) = (show a) ++ "R "
        show (Knight a) = (show a) ++ "Kn"
        show (Bishop a) = (show a) ++ "B "
        show (Pawn a) = (show a) ++ "P "
        show Empty = "   "
    
    data Board = Board [[Piece]]

    defaultBoard :: Board
    defaultBoard = Board 
        [[Rook Black, Knight Black, Bishop Black, Queen Black, King Black, Bishop Black, Knight Black, Rook Black]
        ,replicate  8 (Pawn Black)
        ,replicate  8 (Empty)
        ,replicate  8 (Empty)
        ,replicate  8 (Empty)
        ,replicate  8 (Empty)
        ,replicate  8 (Pawn White)
        ,[Rook White, Knight White, Bishop White, Queen White, King White, Bishop White, Knight White, Rook White]]

    divider :: [Char]
    divider = " +---+---+---+---+---+---+---+---+\n"
    
    instance Show Board where
        show (Board x) =  divider ++ (showHelper x) where
            showHelper xs@(h:t) = (show $ length xs) ++ (showRow h) ++ divider ++ (showHelper t) where
                showRow (h2:t2)  = "|" ++ (show h2) ++ showRow t2
                showRow [] ="|\n"
            showHelper [] = concat ["   "++[j]|j<-['a'..'h']]

    parse :: String -> String
    parse (x:y:z:xs) | z == ' ' = "Yesh!"
                     |otherwise = "No!"
    
    main :: IO ()
    main = print defaultBoard -- $ parse "ab xy"
    