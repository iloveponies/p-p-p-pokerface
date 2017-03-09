(ns p-p-p-pokerface)

(defn rank [[rank _]]
  (let [high-rank-map {\T 10 \J 11 \Q 12 \K 13 \A 14}]
    (if (contains? high-rank-map rank)
      (high-rank-map rank)
      (Integer/valueOf (str rank)))))

(defn suit [[_ suit]]
  (str suit))

(defn hand-frequencies
  [hand mapper]
  (vals (frequencies (map mapper hand))))

(defn x-of-a-kind?
  ([x hand mapper] (boolean (some #(= % x) (hand-frequencies hand mapper))))
  ([x hand] (x-of-a-kind? x hand rank)))

(defn pair? [hand]
  (x-of-a-kind? 2 hand))

(defn three-of-a-kind? [hand]
  (x-of-a-kind? 3 hand))

(defn four-of-a-kind? [hand]
  (x-of-a-kind? 4 hand))

(defn flush? [hand]
  (x-of-a-kind? 5 hand suit))

(defn full-house? [hand]
  (and (pair? hand) (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  (let [rank-frequencies (hand-frequencies hand rank)]
    (boolean (or (four-of-a-kind? hand) (= [2 2] (filter #(= % 2) rank-frequencies))))))

(defn straight? [hand]
  (let [ranks (sort (map rank hand))
        incrementing? #(let [min-rank (apply min %)] (= % (range min-rank (+ min-rank 5))))]
    (cond
      (incrementing? ranks) true
      (= 14 (apply max ranks)) (incrementing? (sort (replace {2 15, 3 16, 4 17, 5 18} ranks)))
      :else false )))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand] true)

(defn value [hand]
  (let [checkers #{[high-card? 0]
                   [pair? 1]
                   [two-pairs? 2]
                   [three-of-a-kind? 3]
                   [straight? 4]
                   [flush? 5]
                   [full-house? 6]
                   [four-of-a-kind? 7]
                   [straight-flush? 8]}]
    (apply max (map (fn [[checker value]] (if (checker hand) value -1)) checkers))))
