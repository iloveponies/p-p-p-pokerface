(ns p-p-p-pokerface)

(defn rank [card]
   (let [[r _] card
          rk (first (replace {\2 2, \3 3, \4 4, \5 5, \6 6, \7 7, \8 8, \9 9, \T 10, \J 11, \Q 12, \K 13, \A 14} [r]))]
     rk))

(defn suit [card]
  (let [[_ s] card
        sk (first  (replace {\H "H", \C "C", \D "D", \S "S"} [s]))]
    sk))

;one pair. aka 4,4, 5, 6, 7
(defn pair? [hand]
  (let [rank-frequency (frequencies (map rank hand))
        pair-values (filter (fn [x]
                                 (= x 2)) (vals rank-frequency))
        one-pair (= (count pair-values) 1)]
    one-pair))

(defn three-of-a-kind? [hand]
  (let [rank-frequency (frequencies (map rank hand))
        three-values (filter (fn [x]
                                 (= x 3)) (vals rank-frequency))]
    (= (count three-values) 1)))

(defn four-of-a-kind? [hand]
   (let [rank-frequency (frequencies (map rank hand))
        four-values (filter (fn [x]
                                 (= x 4)) (vals rank-frequency))]
    (= (count four-values) 1)))

;no-checks for straight
(defn flush? [hand]
  (let [suit-frequency (frequencies (map suit hand))]
    (= 5 (apply min (vals suit-frequency)))))

(defn full-house? [hand]
  (and (pair? hand) (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  (let [rank-frequency (frequencies (map rank hand))
        pair-values (filter (fn [x]
                                 (= x 2)) (vals rank-frequency))]
    (= 2 (count pair-values))))

(defn straight? [hand]
  (let [ranks  (sort (map rank hand))
        ranks1 (sort (replace {14 1} ranks)) 
        min-ranks  (apply min ranks)
        min-ranks1 (apply min ranks1)]
    (or (= ranks  (range min-ranks  (+ 5 min-ranks))) 
        (= ranks1 (range min-ranks1 (+ 5 min-ranks1))))))

(defn straight-flush? [hand]
  (and (flush? hand) (straight? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}
        results (map (fn [checker] [((first checker) hand) (second checker)]) checkers)]
    (apply max (map second (filter (fn [x] (true? (first x))) results)))))

