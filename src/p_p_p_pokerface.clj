(ns p-p-p-pokerface)

(defn rank [card]
  (let [[fst _] card]
    (if (Character/isDigit fst)
      (Integer/valueOf (str fst))
      (get {\T 10, \J 11, \Q 12, \K 13, \A 14} fst)
    )
  )
)

(defn suit [card]
  (let [[_ snd] card]
    (str snd))
)

(defn pair? [hand]
  (contains? (set (vals (frequencies (map rank hand)))) 2)
)

(defn three-of-a-kind? [hand]
  (contains? (set (vals (frequencies (map rank hand)))) 3)
)

(defn four-of-a-kind? [hand]
  (contains? (set (vals (frequencies (map rank hand)))) 4)
)

(defn flush? [hand]
  (contains? (set (vals (frequencies (map suit hand)))) 5)
)

(defn full-house? [hand]
  (and (pair? hand)
       (three-of-a-kind? hand))
)

(defn two-pairs? [hand]
  (let [is-pair? (fn [x] (= x 2))]
    (< 1 (count (filter is-pair? (vals (frequencies (map rank hand)))))))
)

(defn straight? [hand]
  (let [sortedhand (fn [x] (sort (map rank x)))
        sortedhandace (fn [x] (sort (replace {14 1} (map rank x))))]
    (or 
      (= (sortedhand hand) (range (first (sortedhand hand)) (+ (first (sortedhand hand)) 5)))
      (= (sortedhandace hand) (range (first (sortedhandace hand)) (+ (first (sortedhandace hand)) 5)))
    )
  )
)

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand))
  )

(defn high-card? [hand]
  true)

(defn handval [v hand]
  (if ((get v 0) hand)
    (get v 1)
    0
  )
)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}]
    (max
      (handval (get (vec checkers) 0) hand)
      (handval (get (vec checkers) 1) hand)
      (handval (get (vec checkers) 2) hand)
      (handval (get (vec checkers) 3) hand)
      (handval (get (vec checkers) 4) hand)
      (handval (get (vec checkers) 5) hand)
      (handval (get (vec checkers) 6) hand)
      (handval (get (vec checkers) 7) hand)
      (handval (get (vec checkers) 8) hand)
    )
  )
)

; (doseq [i checkers] ((first i) ["2H" "3S" "4C" "5D" "AD"]))
; (if ((first (first checkers)) straight-hand) (second (first checkers)) 0)