(ns p-p-p-pokerface)

(defn rank [card]
  (let [[face _] card
        ranks  {\A 14, \K 13, \Q 12, \J 11, \T 10}]
    (cond (Character/isDigit face) (Integer/valueOf (str face))
          :else                    (get ranks face)
          )
    ))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)
  ))

(defn same-card-values [hand]
          (vals
           (frequencies
            (map rank hand))))

(defn max-number-of-same-cards [hand]
  (apply max (same-card-values hand))
  )


(defn pair? [hand]
  (> (max-number-of-same-cards hand)
     1))

(defn three-of-a-kind? [hand]
  (> (max-number-of-same-cards hand)
     2))

(defn four-of-a-kind? [hand]
  (> (max-number-of-same-cards hand)
     3))

(defn flush? [hand]
  (= (apply max (vals (frequencies (map suit hand))))
     5))

(defn full-house? [hand]
  (= (seq [2 3])
     (sort (same-card-values hand))))

(defn two-pairs? [hand]
  (= (seq [1 2 2])
     (sort (same-card-values hand))))

(defn hand-range-equals? [min-val max-val rng]
  (= (range min-val (inc max-val))
     rng)
  )

(defn straight-sequence? [ranks]
  (hand-range-equals? (reduce min ranks) (reduce max ranks) ranks)
  )

(defn straight? [hand]
  (let [ranks (map rank hand)
        ranks-sorted (sort ranks)
        ranks-sorted-a-low-replaced (sort (replace {14 1} ranks))
        ]
  (or (straight-sequence? ranks-sorted)
      (straight-sequence? ranks-sorted-a-low-replaced)
     )))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}
        matching-checkers (filter (fn [[checker _]] (checker hand)) checkers)
        values-of-matching-checkers (map second matching-checkers)]
   (apply max values-of-matching-checkers)))
