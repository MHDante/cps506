 (ns Assign4)
;;----------UTILS
(defn error [msg] (throw (Exception. msg)))

(defmulti abrv identity)
(defmethod abrv :default [] (error "Symbol not accepted Assign4/abrv."))

(defprotocol Show
 (show [x] "UserReadable string representation")
 )

(extend-type Object
 Show (show [x] (str x))
 )

;;----------COLOR
(def colors #{:Black :White})

(defmethod abrv :Black [] "B")
(defmethod abrv :White [] "W")

(defn negate [sym] (cond
                    (= sym :Black) :White
                    (= sym :White) :Black
                    :else (error "Symbol not accepted Assign4/negate.")
                    )
 )
;;----------END COLOR
(def ranks #{:King :Queen :Rook :Knight :Bishop :Pawn :Empty})
(defmethod abrv :King [] "K")
(defmethod abrv :Queen [] "Q")
(defmethod abrv :Rook [] "R")
(defmethod abrv :Knight [] "Kn")
(defmethod abrv :Bishop [] "B")
(defmethod abrv :Pawn [] "P")
(defmethod abrv :Empty [] "")
;;----------PIECE
(defrecord Piece [rank color]
 Show
 (show [x] (cond
            (= rank :Knight) (str (abrv color) (abrv rank))
            (= rank :Empty) "   "
            :else (str (abrv color) (abrv rank) " ")
            )
  )
 )

;;eatOrOccupy :: Board -> Color -> Coord -> Maybe Action

(defn eatOrOccupy [board col crd] (let [pce (select board crd)]
                                   (cond
                                    (= pce :Nothing) :Nothing
                                    (= (:rank pce) :Empty) '(:Occupy crd)
                                    (not= (:color pce) col) '(:Eat crd)
                                    :else :Nothing
                                    )
                                   )
 )
(defn takeIfOccupied [board col crd] (let [res (eatOrOccupy board col crd)]
                                      (if (= res '(:Occupy crd)) :Nothing res)
                                      )
 )

;;takeIfOccupied :: Board -> Color -> Coord -> Maybe Action

(def dirTable {
               "N"  (0 1)
               "S"  (0 -1)
               "E"  (1 0)
               "W"  (-1 0)
               "NE" (1, 1)
               "NW" (-1, 1)
               "SE" (1 -1)
               "SW" (-1, -1)
               })

;;type Analyzer = Board-> Coord-> Maybe Action
;;getAnalyzer::Piece -> String -> Analyzer

(defn getAnalyzer [pce [a b c d & e]]
  (cond
    (or (= a nil) (= (:rank pce) :Empty)) #(:Nothing)
    (and (= b \space) (= c d e nil)) (getAnalyzer pce (str a " 8"))
    (and (= c \space) (= d e nil)) (getAnalyzer pce (str a b " 8"))
    (and (= d \space) (= e nil)) (getAnalyzer pce (str a b c))
    (= b c d e nil) (getAnalyzer pce (str a " 8"))
    (= c d e nil) (getAnalyzer pce (str b e " 8"))
    (and (= pce (Piece. :Pawn :Black)) (= a \S) (= b \space) (= d e nil))
    (fn [board [x y]] (if (not (contains? "12345678" c))
                        :Nothing
                        (if (and
                              (not= c \1)
                              (= y 6)
                              (= (:rank (select board [x (- y 1)])) (:rank (select board [x (- y 2)])) :Empty)
                            )
                          '(:Occupy [x (- y 2)])
                          (if (= (:rank (select board [x (- y 1)])) :Empty)
                            '(:Occupy [x (- y 1)])
                            :Nothing
                            )
                          )
                        )
      )
    (and (= pce (Piece. :Pawn :White)) (= a \N) (= b \space) (= d e nil))
    (fn [board [x y]] (if (not (contains? "12345678" c))
                        :Nothing
                        (if (and
                              (not= c \1)
                              (= y 1)
                              (= (:rank (select board [x (+ y 1)])) (:rank (select board [x (+ y 2)])) :Empty)
                            )
                          '(:Occupy [x (+ y 2)])
                          (if (= (:rank (select board [x (+ y 1)])) :Empty)
                            '(:Occupy [x (+ y 1)])
                            :Nothing
                            )
                          )
                        )
      )
    (and (= (:rank pce) :Pawn) (= c \space) (contains? "12345678" d))
    (fn [board [x y]] (takeIfOccupied board (:color pce) (cond
                                                           (= (str a b) "SE") [(- x 1) (- y 1)]
                                                           (= (str a b) "SW") [(+ x 1) (- y 1)]
                                                           (= (str a b) "NE") [(- x 1) (+ y 1)]
                                                           (= (str a b) "NW") [(+ x 1) (+ y 1)]
                                                           :else [-1 -1]
                                                           )
                                      )
      )
    (
      = (:rank pce) :Knight)
    (fn [board [x y]] (eatOrOccupy board (:color pce) (cond
                                                        (= (str a b) "NNE") [(+ x 1) (+ y 2)]
                                                        (= (str a b) "NNW") [(- x 1) (+ y 2)]
                                                        (= (str a b) "EEN") [(+ x 2) (+ y 1)]
                                                        (= (str a b) "EES") [(+ x 2) (- y 1)]
                                                        (= (str a b) "SSE") [(+ x 1) (- y 2)]
                                                        (= (str a b) "SSW") [(- x 1) (- y 2)]
                                                        (= (str a b) "WWN") [(- x 2) (+ y 1)]
                                                        (= (str a b) "WWS") [(- x 2) (- y 1)]
                                                        :else [-1 -1]
                                                        )
                                   )
      )
    (
      and (= (:rank pce) :King) (= b \space) (= d e nil))
    (getAnalyzer (Piece. :Queen (:color pce)) (str a " 1"))
    (and (= (:rank pce) :King) (= c \space) (= e nil))
    (getAnalyzer (Piece. :Queen (:color pce)) (str a b " 1"))
    (and (= (:rank pce) :Rook) (= c \space) (= e nil)) #(:Nothing)
    (and (= (:rank pce) :Bishop) (= b \space) (= d e nil)) #(:Nothing)
    (and (= b \space) (= d e nil))
    (if-let [d (get dirTable (str a))]
      (analHelper d (digitToInt c) 1 (:color pce))
      #(:Nothing)
      )
    (and (= c \space) (= e nil))
    (if-let [d (get dirTable (str a b))]
      (analHelper d (digitToInt d) 1 (:color pce))
      #(:Nothing)
      )
    :else #(:Nothing)
    )
  (defn analHelper [[dx dy] limit i c board [x,y]]
    (let [coords [(* (+ x i) dx) (* (+ y i) dy)]]
      (cond
        (> i limit) :Nothing
        (not= (:rank (select board coords)) :Empty) (eatOrOccupy board c coords)
        :else (let [next (analHelper [dx dy] limit (+ i 1) c board [x y])]
                (if (= :Nothing next) (eatOrOccupy board c coords) next)
                )
        )
      )
    )

    ;;----------BOARD
  (defn select [b [x y]] (cond
                           (
                             or (> y (- (count b) 1)) (< y 0) (< x 0) (> x (count (first b)))) :Nothing
                           :else (get (get b y) x)
                           )
    )

  (def divider (apply str (take 33 (cycle "+---"))))
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
