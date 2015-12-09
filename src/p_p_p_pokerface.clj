(ns p-p-p-pokerface)

(defn rank [card]
  (let [[fst _] card]
    (if (Character/isDigit fst)
      (Integer/valueOf (str fst))
      (get {\T 10 \J 11 \Q 12 \K 13 \A 14} fst))))

(defn suit [card]
  (str (second card)))

(defn pair? [hand]
  (let [ranks (map rank hand)
        rank-freqs (set (vals (frequencies ranks)))]
    (boolean (contains? rank-freqs 2))))

(defn three-of-a-kind? [hand]
  (let [ranks (map rank hand)
        rank-freqs (set (vals (frequencies ranks)))]
    (boolean (contains? rank-freqs 3))))

(defn four-of-a-kind? [hand]
  (let [ranks (map rank hand)
        rank-freqs (set (vals (frequencies ranks)))]
    (boolean (contains? rank-freqs 4))))

(defn flush? [hand]
  (= 1 (count (set (map suit hand)))))

(defn full-house? [hand]
  (let [rank-freqs (vals (frequencies (map rank hand)))]
    (= [2 3] (sort rank-freqs))))

(defn two-pairs? [hand]
  (let [rank-freqs (vals (frequencies (map rank hand)))
        pairs-only (filter #(= % 2) rank-freqs)]
    (or (= 2 (count pairs-only))
        (four-of-a-kind? hand))))

(defn straight? [hand]
  (let [straights (set (map #(range % (+ % 5)) (range 1 11)))
        ranks (sort (map rank hand))]
    (or (contains? straights ranks)
        (contains? straights (sort (replace {1 14} ranks)))
        (contains? straights (sort (replace {14 1} ranks))))))

(defn straight-flush? [hand]
  (and (straight? hand)
       (flush? hand)))

(defn high-card? [_hand]
  true)

(defn check-value [checker value hand]
  (if (checker hand) value 0))

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}
        values (map #(check-value (first %) (second %) hand) checkers)]
    (apply max values)))

(defn hand [a-hand]
  a-hand)

(defn card [a-card]
  a-card)