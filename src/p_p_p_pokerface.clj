(ns p-p-p-pokerface)

(defn rank [card]
  (let [rank-map (into {"T" 10 "J" 11 "Q" 12 "K" 13 "A" 14}
                       (for [i (range 2 10)] [(str i) i]))
        c (str (get card 0))]
    (get rank-map c)))

(defn suit [card]
  (str (get card 1)))

(defn pair? [hand]
  (boolean (some #{2} (vals (frequencies (map rank hand)))) ))

(defn three-of-a-kind? [hand]
  (boolean (some #{3} (vals (frequencies (map rank hand)))) ))

(defn four-of-a-kind? [hand]
  (boolean (some #{4} (vals (frequencies (map rank hand)))) ))

(defn flush? [hand]
  (boolean (some #{5} (set (vals (frequencies (map suit hand))))) ))

(defn full-house? [hand]
  (and (pair? hand) (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  (or (four-of-a-kind? hand)
      (= (sort (vals (frequencies (map rank hand)))) [1 2 2])))

(defn straight? [hand]
  (let [sorted-rank (sort (map rank hand))
        sorted-rank1 (sort (replace {14 1} sorted-rank))
        accend (fn [rank]
                 (= (map #(- %2 %1) rank (rest rank))
                    [1 1 1 1]))]
    (or (accend sorted-rank)
        (accend sorted-rank1))))


(defn straight-flush? [hand]
  (and (straight? hand)
       (flush? hand)))

(defn high-card? [hand]
  (and (= (vals (frequencies (map rank hand)))
          [1 1 1 1 1])
       (not (straight? hand))
       (not (flush? hand))))

(defn value [hand]
  (let [checkers #{[high-card? 0] 
                   [pair? 1] 
                   [two-pairs? 2] 
                   [three-of-a-kind? 3] 
                   [straight? 4] 
                   [flush? 5]
                   [full-house? 6] 
                   [four-of-a-kind? 7] 
                   [straight-flush? 8]}
        check-res (map #(if ((first %) hand)
                            (second %)
                            -1)
                         checkers)]
    (apply max check-res)
    ))
