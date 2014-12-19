(ns p-p-p-pokerface)

(defn hand [a-hand]
  (map card a-hand))

(defn card [a-card]
  {:rank (rank a-card) :suit (suit a-card)})

(defn rank [[fst _]]
  (let [rankOfChar (fn [x] (get {\T 10, \J 11, \Q 12, \K 13, \A 14} x))]
    (if (Character/isDigit fst)
      (Integer/valueOf (str fst))
      (rankOfChar fst))))

(defn suit [[_ snd]]
  snd)

(defn freqs [hand]
  (vals (frequencies (map rank hand))))

(defn pair? [hand]
  (>= (apply max (freqs hand)) 2))

(defn three-of-a-kind? [hand]
  (>= (apply max (freqs hand)) 3))

(defn four-of-a-kind? [hand]
  (>= (apply max (freqs hand)) 4))

(defn flush? [hand]
  (apply = (map suit hand)))

(defn full-house? [hand]
  (let [counts (sort (freqs hand))]
    (= counts '(2 3))))

(defn two-pairs? [hand]
  (let [counts (freqs hand)
        pairs (filter (fn [x] (>= x 2)) counts)]
    (or (>= (count pairs) 2)
        (four-of-a-kind? hand))))

(defn straight? [hand]
  (let [ranks (sort (map rank hand))
        lowranks (sort (replace {14 1} ranks))
        start-high (first ranks)
        start-low (first lowranks)
        compare (fn [rs s] (= rs (range s (+ s 5))))]
    (or (compare ranks start-high)
        (compare lowranks start-low))))

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
                   [straight-flush? 8]}
        matches (filter (fn [checker]
                          ((first checker) hand)) checkers)
        values (map second matches)]
    (apply max values)))
