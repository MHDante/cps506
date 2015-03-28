(ns Assign4
  (:gen-class))
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

(declare select analHelper digitToInt parse updateState)

(defn nullify
  ([] nil)
  ([& _] nil))

(defn digitToInt [chr] (- (int chr) (int \0)))
;;----------COLOR
(def colors #{:Black :White})

(defmethod abrv :Black [_] "B")
(defmethod abrv :White [_] "W")

(defn negate [sym] (cond
                     (= sym :Black) :White
                     (= sym :White) :Black
                     :else (error "Symbol not accepted Assign4/negate.")
                     )
  )
;;----------END COLOR
(def ranks #{:King :Queen :Rook :Knight :Bishop :Pawn :Empty})
(defmethod abrv :King [_] "K")
(defmethod abrv :Queen [_] "Q")
(defmethod abrv :Rook [_] "R")
(defmethod abrv :Knight [_] "Kn")
(defmethod abrv :Bishop [_] "B")
(defmethod abrv :Pawn [_] "P")
(defmethod abrv :Empty [_] "")
;;----------PIECE
(defrecord Piece [rank color]
  Show
  (show [_] (cond
              (= rank :Knight) (str (abrv color) (abrv rank))
              (= rank :Empty) "   "
              :else (str (abrv color) (abrv rank) " ")
              )
    )
  )

;;eatOrOccupy :: Board -> Color -> Coord -> Maybe Action

(defn eatOrOccupy [board col crd] (let [pce (select board crd)]
                                    (cond
                                      (nil? pce) nil
                                      (= (:rank pce) :Empty) [:Occupy crd]
                                      (not= (:color pce) col) [:Eat crd]
                                      :else nil
                                      )
                                    )
  )
(defn takeIfOccupied [board col crd] (do (println (str "This one" crd))(let [res (eatOrOccupy board col crd)]
                                       (if (= res [:Occupy crd]) nil res)
                                       ))
  )

;;takeIfOccupied :: Board -> Color -> Coord -> Maybe Action

(def dirTable {
               "N"  [0 1]
               "S"  [0 -1]
               "E"  [1 0]
               "W"  [-1 0]
               "NE" [1 1]
               "NW" [-1 1]
               "SE" [1 -1]
               "SW" [-1 -1]
               })

;;type Analyzer = Board-> Coord-> Maybe Action
;;getAnalyzer::Piece -> String -> Analyzer

(defn getAnalyzer [pce [a b c d & e]]
  (cond
    (or (= a nil) (= (:rank pce) :Empty)) nullify
    (and (= b \space) (= c d e nil)) (getAnalyzer pce (str a " 8"))
    (and (= c \space) (= d e nil)) (getAnalyzer pce (str a b " 8"))
    (and (= d \space) (= e nil)) (getAnalyzer pce (str a b c))
    (= b c d e nil) (getAnalyzer pce (str a " 8"))
    (= c d e nil) (getAnalyzer pce (str a b " 8"))
    (= (:rank pce) :Pawn)
    (cond
      (and (= b \space) (= d e nil))
      (do (println (str a ":" b ":"c":"d":"e))(fn pawnMove [board [x y]]
        (if (or (not (contains? (set "12345678") c)) (not= (if (= (:color pce) :Black) \S \N) a))
          nil
          (if (and
                (not= c \1)
                (= y (if (= (:color pce) :Black) 6 1))
                (=
                  (:rank (select board [x (if (= (:color pce) :Black) (- y 1) (+ y 1))]))
                  (:rank (select board [x (if (= (:color pce) :Black) (- y 2) (+ y 2))]))
                  :Empty
                  )
                )
            [:Occupy [x (if (= (:color pce) :Black) (- y 2) (+ y 2))]]
            (if (= (:rank (select board [x (if (= (:color pce) :Black) (- y 1) (+ y 1))])))
                   [:Occupy [x (if (= (:color pce) :Black) (- y 1) (+ y 1))]]
                   nil
                   )
              )
            )
          ))
      (= c \space)
      (fn pawnEat [board [x y]]
        (takeIfOccupied board (:color pce)
                        (cond
                          (and (= (:color pce) :Black) (= (str a b) "SE")) [(+ x 1) (- y 1)]
                          (and (= (:color pce) :Black) (= (str a b) "SW")) [(- x 1) (- y 1)]
                          (and (= (:color pce) :White) (= (str a b) "NE")) [(+ x 1) (+ y 1)]
                          (and (= (:color pce) :White) (= (str a b) "NW")) [(- x 1) (+ y 1)]
                          :else [-1 -1]
                          )
                        )
        )
      :else nullify
      )
    (
      = (:rank pce) :Knight)
    (fn knightMove [board [x y]] (eatOrOccupy board (:color pce) (cond
                                                                   (= (str a b c) "NNE") [(+ x 1) (+ y 2)]
                                                                   (= (str a b c) "NNW") [(- x 1) (+ y 2)]
                                                                   (= (str a b c) "EEN") [(+ x 2) (+ y 1)]
                                                                   (= (str a b c) "EES") [(+ x 2) (- y 1)]
                                                                   (= (str a b c) "SSE") [(+ x 1) (- y 2)]
                                                                   (= (str a b c) "SSW") [(- x 1) (- y 2)]
                                                                   (= (str a b c) "WWN") [(- x 2) (+ y 1)]
                                                                   (= (str a b c) "WWS") [(- x 2) (- y 1)]
                                                                   :else (do (println "happen") [-1 -1])
                                                                   )
                                              )
      )
    (
      and (= (:rank pce) :King) (= b \space) (= d e nil))
    (getAnalyzer (Piece. :Queen (:color pce)) (str a " 1"))
    (and (= (:rank pce) :King) (= c \space) (= e nil))
    (getAnalyzer (Piece. :Queen (:color pce)) (str a b " 1"))
    (and (= (:rank pce) :Rook) (= c \space) (= e nil)) nullify
    (and (= (:rank pce) :Bishop) (= b \space) (= d e nil)) nullify
    (and (= b \space) (= d e nil))
    (if-let [dir (get dirTable (str a))]
      (do (println dir) (analHelper dir (digitToInt c) 1 (:color pce)))
      nullify
      )
    (and (= c \space) (= e nil))
    (if-let [dir (get dirTable (str a b))]
      (analHelper dir (digitToInt d) 1 (:color pce))
      nullify
      )
    :else nullify
    )
  )
(defn analHelper [[dx dy] limit i c]
  (fn ahh [board [x y]]
    (let [coords [(+ x (* i dx)) (+ y (* i dy))]]
      (cond
        (> i limit) nil
        (not= (:rank (select board coords)) :Empty) (eatOrOccupy board c coords)
        :else (let [next ((analHelper [dx dy] limit (+ i 1) c) board [x y])]
                (if (nil? next) (eatOrOccupy board c coords) next)
                )
        )
      )
    )
  )
;;----------BOARD
(defn select [b [x y]] (if
                         (or
                           (>= y 8)
                           (< y 0)
                           (< x 0)
                           (>= x 8)
                           )
                         (do (println (str "Out of Bounds" x " " y)) nil)
                         (do (println (str "In Bounds" x " " y))(nth (nth b y) x))
                         )
  )

(def divider (apply str " " (take 33 (cycle "+---"))))
(def letters (apply str (map #(str "   " %) "abcdefgh")))


(defrecord BoardState [board blackCaps whiteCaps turn message]
  Show
  (show [_] (letfn [
                    (msg [i] (cond
                               (= i 8) (str "  White Captures: " (apply str (map show whiteCaps)) \newline)
                               (= i 6) (str "  Black Captures: " (apply str (map show blackCaps)) \newline)
                               (= i 4) (str "  Current Turn: " turn \newline)
                               (= i 2) (str "  " message \newline)
                               :else \newline
                               )
                         )
                    (showRow [x]
                             (if (empty? x)
                               "|\n"
                               (str "|" (show (first x)) (showRow (rest x)))
                               )
                             )
                    (showHelper [[h & t :as xs] i]
                                (if (empty? xs)
                                  ""
                                  (str (showHelper t (+ i 1)) i (showRow h) divider (msg i))
                                  )
                                )
                    ]
              (str divider \newline (showHelper board 1) letters)
              )
    )
  )
;;----------END BOARD

(defn applyMove [{:keys [board blackCaps whiteCaps turn message] :as bs} mv]
  (if-let [res (parse board mv)]
    (let [[pce coord anlz ] res] ;(Empty, (0,0), defAnalyzer)
      (cond
        (= (:rank pce) :Empty) (BoardState. board blackCaps whiteCaps turn "Could Not Select Piece")
        (not= (:color pce) turn) (BoardState. board blackCaps whiteCaps turn (str "Not your Turn! " (:keys bs)))
        :else (let [act (anlz board coord)]
                (if (do (println act) (nil? act))
                  (BoardState. board blackCaps whiteCaps turn "Could Not Perform Move")
                  (updateState bs act coord)
                  )
                )
        )
      )
    (BoardState. board blackCaps whiteCaps turn "Could Not Select Piece...")
    )
  )

(defn updateState [{:keys [board blackCaps whiteCaps turn message]} [action [x1 y1]] [x0 y0]]
  (let [pce (select board [x0 y0])]
    (letfn [
            (updateRow [[h2 & t2 :as xs2] x3 y3]
                       (if (empty? xs2)
                         []
                         (cons (cond
                                 (and (= x1 x3) (= y1 y3)) (do (println (str "Move!" x3 " " y3 " " pce)) pce)
                                 (and (= x0 x3) (= y0 y3)) (do (println (str "Empty!" x1 " " y1)) (Piece. :Empty nil))
                                 :else h2
                                 )
                               (updateRow t2 (+ x3 1) y3)
                               )
                         )
                       )
            (updateBoard [[h1 & t1 :as xs1] x2 y2]
                         (if (empty? xs1)
                           []
                           (cons (updateRow h1 x2 y2) (updateBoard t1 x2 (+ y2 1)))
                           )
                         )
            ]
      (cond
        (= action :Eat) (BoardState.
                          (updateBoard board 0 0)
                          (if (= turn :Black) blackCaps (conj blackCaps pce))
                          (if (= turn :White) whiteCaps (conj whiteCaps pce))
                          (negate turn)
                          (str (show pce) " ate " (show (select board [x1 y1])))
                          )
        (= action :Occupy) (BoardState. (updateBoard board 0 0) blackCaps whiteCaps (negate turn) (str (show pce) " moved"))
        )
      )
    )
  )

(defn parse [board [a n s & xs]]
  (let [coord [(- (int a) (int \a)) (dec (digitToInt n))]
        pce (select board coord)]
    (if (or
          (nil? pce)
          (not= s \space)
          (not (contains? (set "abcdefgh") a))
          (not (contains? (set "12345678") n))
          (empty? xs)
          )
      (do (println coord) nil)
      [pce, coord (getAnalyzer pce xs)]
      )
    )
  )
(def defaultBoard [
                   [(Piece. :Rook :White)
                    (Piece. :Knight :White)
                    (Piece. :Bishop :White)
                    (Piece. :Queen :White)
                    (Piece. :King :White)
                    (Piece. :Bishop :White)
                    (Piece. :Knight :White)
                    (Piece. :Rook :White)
                    ]
                   (replicate 8 (Piece. :Pawn :White))
                   (replicate 8 (Piece. :Empty nil))
                   (replicate 8 (Piece. :Empty nil))
                   (replicate 8 (Piece. :Empty nil))
                   (replicate 8 (Piece. :Empty nil))
                   (replicate 8 (Piece. :Pawn :Black))
                   [(Piece. :Rook :Black)
                    (Piece. :Knight :Black)
                    (Piece. :Bishop :Black)
                    (Piece. :Queen :Black)
                    (Piece. :King :Black)
                    (Piece. :Bishop :Black)
                    (Piece. :Knight :Black)
                    (Piece. :Rook :Black)
                    ]
                   ]
  )



(defn main2 [board]
  (do
    (println (show board))
    (let [b2 (applyMove board (read-line))] (recur b2))
    )
  )

(defn -main
  [& _] (main2 (BoardState. defaultBoard [] [] :White ""))
  )