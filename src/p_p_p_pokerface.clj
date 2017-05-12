(ns p-p-p-pokerface)

; helpers

(defn digit? [d]
  (Character/isDigit d)
)

(def replacements {\A 14, \K 13, \Q 12, \J 11, \T 10})


; helpers

(defn rank [card]
  (let [[c _] card]
   (cond
     (Character/isDigit  c) (Integer/valueOf (str c))
     :else (replacements c))
    )
)

(defn suit [card]
  (let [[_ snd] card]
  (str snd))
)

(defn pair? [hand]
  (cond
  (< 1 (apply max (vals (frequencies (map rank hand))))) true
    :else false)
)

(defn three-of-a-kind? [hand]
  (cond
  (< 2 (apply max (vals (frequencies (map rank hand))))) true
    :else false)
)

(defn four-of-a-kind? [hand]
  (cond
  (< 3 (apply max (vals (frequencies (map rank hand))))) true
    :else false)
)

(defn flush? [hand]
  (cond
  (< 4 (apply max (vals (frequencies (map suit hand))))) true
    :else false)
)

(defn full-house? [hand]
  (let [fval  (first (vals (frequencies (map rank hand))))]
    fval
    (let [sval  (second (vals (frequencies (map rank hand)))) ]
     (cond
       (and (= 2 sval) (= 3 fval)) true
       :else false)
      )
    )
)

(defn two-pairs? [hand]
  (let [fval  (first (vals (frequencies (map rank hand))))]
    fval
    (let [sval  (second (vals (frequencies (map rank hand)))) ]
      (cond
        (or (= 4 fval) (and (= 2 sval) (= 2 fval))) true
        :else false)
      )
    )
)

(defn straight? [hand]
  (let [ma (apply max (map rank hand))]
    (let [mi (apply min (map rank hand))]
      (let [althand (replace {14 1} (sort (map rank hand))) ]
        (let [altmin (apply min althand)]
          (let [altmax (apply max althand)]
      (or  (= (sort althand) (range altmin (+ 1 altmax)))(= (sort (map rank hand)) (range mi (+ 1 ma))))
      )))))
)

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand))
)

(defn value [hand]
   (cond
    (straight-flush? hand) 8
    (four-of-a-kind? hand) 7
    (full-house? hand) 6
    (flush? hand) 5
    (straight? hand) 4
    (three-of-a-kind? hand) 3
    (two-pairs? hand) 2
    (pair? hand) 1
    :else 0)
)
