(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rnk _] card]
    (if (Character/isDigit rnk)
      (Integer/valueOf (str rnk))
      (let [replacements {"T" 10, "J" 11, "Q" 12, "K" 13, "A" 14}]
        (replacements (str rnk)))
    )))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn pair? [hand]
  (< 1 (apply max (vals (frequencies (map rank hand))))))

(defn three-of-a-kind? [hand]
  (< 2 (apply max (vals (frequencies (map rank hand))))))

(defn four-of-a-kind? [hand]
  (< 3 (apply max (vals (frequencies (map rank hand))))))

(defn flush? [hand]
  (= 5 (apply max (vals (frequencies (map suit hand))))))

(defn full-house? [hand]
  (and (= 3 (apply max (vals (frequencies (map rank hand)))))
    (= 2 (apply min (vals (frequencies (map rank hand)))))))

(defn two-pairs? [hand]
  (or (four-of-a-kind? hand)
      (= 2 (get (frequencies (vals (frequencies (map rank hand)))) 2))))

(defn sequential-ranks? [ranks]
  (let [minimum (apply min ranks)
        sorted (sort ranks)
        expected (range minimum (+ 5 minimum))]
    (= sorted expected)
  ))

(defn straight? [hand]
  (let [card-vals (map rank hand)]
    (or
      (sequential-ranks? card-vals)
      (sequential-ranks? (replace {14 1} card-vals))
    ))
  )

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checks #{[high-card? 0]
                 [pair? 1]
                 [two-pairs? 2]
                 [three-of-a-kind? 3]
                 [straight? 4]
                 [flush? 5]
                 [full-house? 6]
                 [four-of-a-kind? 7]
                 [straight-flush? 8]}]
    (apply max (map second (filter (fn[x] ((first x) hand)) checks)))
  ))
