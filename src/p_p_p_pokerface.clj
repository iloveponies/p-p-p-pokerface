(ns p-p-p-pokerface)

(def ranks {\2 2 \3 3 \4 4 \5 5 \6 6 \7 7 \8 8 \9 9 \T 10 \J 11 \Q 12 \K 13 \A 14})


(defn rank [card]
  (let [[R] card]
    (ranks R)
    ))

(defn suit [card]
  (let [[_ sec] card]
    (str sec)
    ))

(defn pair? [hand]
  (= 4 (count (frequencies (map rank hand)))))

(defn three-of-a-kind? [hand]
  (and
    (= 3 (apply max (vals (frequencies (map rank hand)))))
    (= 1 (apply min (vals (frequencies (map rank hand)))))
    ))

(defn four-of-a-kind? [hand]
  (= 4 (apply max (vals (frequencies (map rank hand))))))

(defn flush? [hand]
  (= 5 (apply max (vals (frequencies (map suit hand))))))

(defn full-house? [hand]
   (and
    (= 3 (apply max (vals (frequencies (map rank hand)))))
    (= 2 (apply min (vals (frequencies (map rank hand)))))
    ))

(defn two-pairs? [hand]
  (or
    (and
      (= 3 (count (frequencies (map rank hand))))
      (= 2 (apply max (vals (frequencies (map rank hand)))))
     )
    (four-of-a-kind? hand)
      ))

(defn straight? [hand]
  (and
    (or
    (= 4 (-
           (apply max (map rank hand))
           (apply min (map rank hand)))
    )
    (= 28 (apply + (map rank hand)))
    )
    (= 5 (count (frequencies (map rank hand))))
     ))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]
                   [pair? 1]
                   [two-pairs? 2]
                   [three-of-a-kind? 3]
                   [straight? 4]
                   [flush? 5]
                   [full-house? 6]
                   [four-of-a-kind? 7]
                   [straight-flush? 8]
                   }]
    (apply max (map (fn [check] (if ((first check) hand) (second check) 0)) checkers))
    ))


