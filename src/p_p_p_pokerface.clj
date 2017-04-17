(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rank, _] card
        rank-map {\T 10, \J 11, \Q 12, \K 13, \A 14}]
    (if (Character/isDigit rank)
      (Integer/valueOf (str rank))
      (rank-map rank))))

(defn suit [card]
  (let [[_, suit] card]
    (str suit)))

(defn pair? [hand]
  (let [ranks (map rank hand)]
    (>= (apply max (vals (frequencies ranks))) 2)))

(defn three-of-a-kind? [hand]
  (let [ranks (map rank hand)]
    (>= (apply max (vals (frequencies ranks))) 3)))

(defn four-of-a-kind? [hand]
  (let [ranks (map rank hand)]
    (>= (apply max (vals (frequencies ranks))) 4)))

(defn flush? [hand]
  (let [suits (map suit hand)]
    (>= (apply max (vals (frequencies suits))) 5)))

(defn full-house? [hand]
  (let [ranks (map rank hand)]
    (= (sort (vals (frequencies ranks))) [2 3])))

(defn two-pairs? [hand]
  (let [ranks (map rank hand)
        rank-freqs (reverse (sort (vals (frequencies ranks))))]
    (cond
      (>= (first rank-freqs) 4) true
      (>= (first rank-freqs) (nth rank-freqs 1) 2) true
      :else false)))

(defn straight? [hand]
  (let [ranks (map rank hand)
        aced-ranks (sort (if (= (apply min ranks) 2)
                           (replace {14 1} ranks)
                           ranks))]
    (and (= (- (apply max aced-ranks) (apply min aced-ranks)) 4)
         (apply < aced-ranks))))

(defn straight-flush? [hand]
  (and (flush? hand)
       (straight? hand)))

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
    :else 0))

