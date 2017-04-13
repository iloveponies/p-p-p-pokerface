(ns p-p-p-pokerface)

(defn rank [card]
  (let [ [rank _] card]
  ({\2 2 \3 3 \4 4 \5 5 \6 6 \7 7 \8 8 \9 9 \T 10 \J 11 \Q 12 \K 13 \A 14} rank)))

(defn suit [card]
  (let [ [_ suit] card]
  (str suit)))

(defn pair? [hand]
  (not (apply distinct? (map rank hand))))

(defn three-of-a-kind? [hand]
  (contains? (set (vals (frequencies (map rank hand)))) 3))

(defn four-of-a-kind? [hand]
  (contains? (set (vals (frequencies (map rank hand)))) 4))

(defn flush? [hand]
  (= 1 (count (set (map suit hand)))))

(defn full-house? [hand]
  (= #{3 2} (set (vals (frequencies (map rank hand))))))

(defn two-pairs? [hand]
  (let [card-ranks               (map rank hand)
        card-rank-freqs          (frequencies card-ranks)
        card-rank-freq-vals      (vals card-rank-freqs)
        card-rank-freq-val-freqs (frequencies card-rank-freq-vals)]
    (or (= 1 (get card-rank-freq-val-freqs 4 ))
        (= 2 (get card-rank-freq-val-freqs 2 )))))

(defn straight? [hand]
  (let [all-ranks (map rank hand)
        max-card (apply max all-ranks)
        min-card (apply min all-ranks)
        all-ranks-lo (map #(if (= 14 (rank %1)) 1 (rank %1)) hand)
        max-card-lo (apply max all-ranks-lo)
        min-card-lo (apply min all-ranks-lo)]
    (and
      (or (= 4 (- max-card min-card))
          (= 4 (- max-card-lo min-card-lo)))
      (= 5 (count (distinct all-ranks))))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn value [hand]
  (cond (straight-flush? hand)  8
        (four-of-a-kind? hand)  7
        (full-house? hand)      6
        (flush? hand)           5
        (straight? hand)        4
        (three-of-a-kind? hand) 3
        (two-pairs? hand)       2
        (pair? hand)            1
        :else 0))
