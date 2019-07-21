(ns p-p-p-pokerface)

(defn rank [card]
  (let [[fst _] card
        facecard-map {\T 10 \J 11 \Q 12 \K 13 \A 14}
        ]
    (cond
     (Character/isDigit fst) (Integer/valueOf (str fst))
     :else (get facecard-map fst)
     )
    ))

(defn suit [card]
  (let [[_ scd] card]
    (str scd)
    ))

(defn pair? [hand]
  (>= (apply max (vals (frequencies (map rank hand))))
      2))

(defn three-of-a-kind? [hand]
  (>= (apply max (vals (frequencies (map rank hand))))
      3))

(defn four-of-a-kind? [hand]
  (>= (apply max (vals (frequencies (map rank hand))))
      4))

(defn flush? [hand]
  (>= (apply max (vals (frequencies (map suit hand))))
      5))

(defn full-house? [hand]
  (= (range 2 4) (sort (vals (frequencies (map rank hand))))))

(defn two-pairs? [hand]
  (let [freq-counts (sort (vals (frequencies (map rank hand))))]
  (cond
   (four-of-a-kind? hand) true
   (= (seq [1 2 2]) freq-counts) true
   :else false
   )))

(defn straight? [hand]
  (let [card-vals (sort (map rank hand))
        max-card (apply max card-vals)
        min-card (apply min card-vals)
        card-vals-ace-subbed (sort (replace {14 1} card-vals))
        max-card-sub (apply max card-vals-ace-subbed)
        min-card-sub (apply min card-vals-ace-subbed)
        ]
    (cond
     (= (range min-card (+ 1 max-card)) card-vals) true
     (= (range min-card-sub (+ 1 max-card-sub)) card-vals-ace-subbed) true
     :else false)))

(defn straight-flush? [hand]
  (and (straight? hand)
       (flush? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers [[high-card? 0]  [pair? 1]
                   [two-pairs? 2]  [three-of-a-kind? 3]
                   [straight? 4]   [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]]]
    (apply max (map second (filter (fn [x] ((first x) hand)) checkers)))
    ))
