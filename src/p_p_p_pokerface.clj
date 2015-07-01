(ns p-p-p-pokerface)

(def face-card-map {\T 10, \J 11, \Q 12, \K 13, \A 14})

(defn rank [card]
  (let [[card-rank] card]
    (if (Character/isDigit card-rank)
      (Integer/valueOf (str card-rank))
      (Integer/valueOf (str (get face-card-map card-rank))))))

(defn suit [card]
  (let [[_ card-suit] card]
    (str card-suit)))

(defn pair? [hand]
  (let [hand-ranks (map rank hand)
        rank-frequency (frequencies hand-ranks)]
    (contains? (set (vals rank-frequency)) 2)))

(defn three-of-a-kind? [hand]
  (let [hand-ranks (map rank hand)
        rank-frequency (frequencies hand-ranks)]
    (contains? (set (vals rank-frequency)) 3)))

(defn four-of-a-kind? [hand]
  (let [hand-ranks (map rank hand)
        rank-frequency (frequencies hand-ranks)]
    (contains? (set (vals rank-frequency)) 4)))

(defn flush? [hand]
  (let [hand-suit (frequencies (map suit hand))]
    (contains? (set (vals hand-suit)) 5)))

(defn full-house? [hand]
  (let [hand-rank (map rank hand)
        rank-frequency (sort (vals (frequencies hand-rank)))]
    (= rank-frequency  '(2 3))))

(defn two-pairs? [hand]
  (let [hand-rank (map rank hand)
        rank-frequency (sort (vals (frequencies hand-rank)))]
    (= rank-frequency  '(1 2 2))))

(defn straight? [hand]
  (let [hand-rank (sort (map rank hand))
        hand-rank-alt (sort (replace {14 1} hand-rank))
        lowest-rank (first hand-rank)
        lowest-rank-alt (first hand-rank-alt)]
    (or (= hand-rank-alt
            (range lowest-rank-alt (+ lowest-rank-alt 5)))
        (= hand-rank
            (range lowest-rank (+ lowest-rank 5))))))

(defn straight-flush? [hand]
  (and (straight? hand)
        (flush? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checker #{[high-card? 0] [pair? 1]
                    [two-pairs? 2] [three-of-a-kind? 3]
                    [straight? 4] [flush? 5]
                    [full-house? 6] [four-of-a-kind? 7]
                    [straight-flush? 8]}
        values (filter (fn [x]
          ((first x) hand)) checker)]
    (apply max
            (map second values))))
