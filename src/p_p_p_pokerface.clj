(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rank _] card arvo {\T 10, \J 11, \Q 12, \K 13, \A 14}]
    (cond
     (Character/isDigit rank) (Integer/valueOf (str rank))
     :else (get arvo rank)
     )
    )
  )

(defn suit [card]
  (let [[_ suit] card]
    (str suit))
  )

(defn pair? [hand]
  (<= 2 (apply max (vals (frequencies (map rank hand)))))
    )

(defn three-of-a-kind? [hand]
  (<= 3 (apply max (vals (frequencies (map rank hand))))))

(defn four-of-a-kind? [hand]
  (<= 4 (apply max (vals (frequencies (map rank hand))))))

(defn flush? [hand]
  (= 5 (apply max (vals (frequencies (map suit hand))))))

(defn full-house? [hand]
  (let [amounts (vals (frequencies (map rank hand)))
        mini (apply min amounts)
        maxi (apply max amounts)]
    (and (= 2 mini) (= 3 maxi))
  )
  )

(defn two-pairs? [hand]
  (let [similars-map (frequencies (vals (frequencies (map rank hand))))]
    (if
     (nil? (get similars-map 2))
     false
    (<= 2 (get similars-map 2))
  )
  ))

(defn straight? [hand]
  (let [ordered (sort (map rank hand))
        alt (sort(replace {14 1} ordered))
        is-straight? (fn [ord] (= ord (range (apply min ord) (+ 1 (apply max ord)))))]
    (or
     (is-straight? ordered)
     (is-straight? alt)
     )
    )
  )

(defn straight-flush? [hand]
  (and
   (straight? hand)
   (flush? hand)
       )
  )

(defn value [hand]
  (let [high-card? (fn [hand] true)
        checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}]
    (apply max (map second (filter (fn [[checker _]] (checker hand)) checkers)))
   )
  )
