(defn hello [] "Hello world")

(hello)

(defn mMap [d x]
      (let [[k & ks] x]
           (do (println (str "k " k " ks " ks " d " d)) (if (= ks nil)
                 {k d}
                 (conj {k d} (mMap d ks))
                 )
               )
      )
)
(defn mRev [x] (let [n (count x)] (if (= n 0) nil (cons (last x) (mRev (take (- n 1) x))))))
(defn palin [x] (let [n (count x)] (if (< n 2) true (and (= (first x) (last x)) (palin (take (- n 2) (drop 1 x)))))))
(fn fib [x] (loop [i 0 r [] ]
                   (if (= i x)
                     r
                     (if (< i 2)
                       (recur (inc i) (conj 1 r))
                              (recur (inc i) (conj (+ (nth (- i 2) r) (last r)) r))
                       )
                     )
                  )
    )

(defn mFlatten [[x & xs]]
          (if-not xs
            (if (coll? x)
              (mFlatten x)
              (cons x nil)
            )
            (if (coll? x)
              (concat (mFlatten x) (mFlatten xs))
              (cons x (mFlatten xs))
              )
            )
    )

(defn mInterleave [c1 c2]
      (loop [i (- (min (count c1) (count c2)) 1) r []]
            (if (< i 0)
              r
              (recur (dec i) (cons (nth c1 i) (cons (nth c2 i) r)))
            )
      )
)

(defn unique [x]
      (loop [i 0 c nil r []]
            (if (= i (count x))
              r
              (if (or (= i 0) (not= c (nth x i)))
                (recur (inc i) (nth x i) (concat r [(nth x i)]))
                (recur (inc i) c r)
                )
              )
            )
      )
(defn sep [s [h & t]] (if t (concat (concat [h] [s]) (sep s t)) [h]))

(defn pack [x]
      (loop [i 1 c [(first x)] r nil]
            (if (= i (count x))
              (concat r [c])
              (if (not= (first c) (nth x i))
                (recur (inc i) [(nth x i)] (concat r [c]))
                (recur (inc i) (conj c (nth x i)) r)
                )
              )
            )
      )

(defn mNth [xs n] (mapcat (fn [a] [(get xs a)]) (for [x (range) :when (not= (rem (+ x 1) n) 0) :while (< x (count xs))] x)))

(defn mIntersect [s1 s2]
    (loop [s s1 r #{}]
          (if (empty? s)
            r
            (if (contains? s2 (first s))
              (recur (rest s) (conj r (first s)))
              (recur (rest s) r)
              )
            )
          )
    )


(defn exp [x n]
      (reduce * (repeat n x)))

(defn digM [x y] ((fn dig [n r] (if (= n 0) r (dig (quot n 10) (conj r (rem n 10)))) ) (* x y) `()))

(defn cart [c1 c2] (set (reduce concat (for [x c1] (for [y c2] [x y])))))

(defn mBin [x] ((fn bin [v t i] (if (empty? i) t (if (= (last i) \1) (bin (* 2 v) (+ t v) (butlast i)) (bin (* 2 v) t (butlast i))))) 1 0 x))

(defn mDot [x y] (reduce + (map #(* (x %) (y %)) (range 0 (count x) ))))

(defn sqr [col] (count (filter (fn [a] (< a
                (reduce + (map #(* % %)
                       ((fn dig [n r] (if (= n 0) r (dig (quot n 10) (conj r (rem n 10))))) a [])
                               )))) col)))

(defn cardparse [s] (let [suit (assoc nil :suit (cond
                                      (= (get s 0) \D) :diamond
                                      (= (get s 0) \H) :heart
                                      (= (get s 0) \C) :club
                                      (= (get s 0) \S) :spade))]
             (assoc suit :rank (cond
                                 (= (get s 1) \T) 8
                                 (= (get s 1) \J) 9
                                 (= (get s 1) \Q) 10
                                 (= (get s 1) \K) 11
                                 (= (get s 1) \A) 12
                                  :else (- (int (get s 1)) 50)))))


(defn pascalizer [l] (loop [k 1 r [(first l)]]
                         (if (= k (count l))
                           (conj r (last l))
                           (recur (inc k) (conj r (+ (nth l k) (nth l (- k 1)))))
                           )
                         )
    )

(defn pasTrap [res] (lazy-seq (cons res (pascalizer (last res)))))

(defn palin [x] (let [n (count x)] (if (< n 2) true (and (= (first x) (last x)) (palin (take (- n 2) (drop 1 x)))))))

(defn prefix [x] (if (= nil x) [nil] (let [[v l r] x] (concat (prefix l) [v] (prefix r)))))

(defn flatMap [m] (apply assoc {} (apply concat (for [[h t] m] (apply concat (for [[a b] t] [[h a] b]))))))

(defn pwsDjt [s]
      (loop [u #{} r s]
           (cond
               (empty? r) true
               (not(empty? (clojure.set/intersection (first r) u))) false
               :else (recur (clojure.set/union (first r) u) (rest r))
           )
      )
)