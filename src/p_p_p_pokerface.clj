(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rank _] card
      ranks {\T 10 \J 11 \Q 12 \K 13 \A 14}]
    (cond
     (Character/isDigit rank) (Integer/valueOf (str rank))
     :else (get ranks rank))))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn frequency-of-hand [frequency hand]
  (contains? (set (vals (frequencies (map (fn [card] (rank card)) hand)))) frequency))

(defn pair? [hand]
  (frequency-of-hand 2 hand))

(defn three-of-a-kind? [hand]
  (frequency-of-hand 3 hand))

(defn four-of-a-kind? [hand]
  (frequency-of-hand 4 hand))

(defn flush? [hand]
  (= (count (vals (frequencies (map (fn [card] (suit card)) hand)))) 1))

(defn full-house? [hand]
  (and (pair? hand) (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  (or (four-of-a-kind? hand)
      (= (count (filter (fn [x] (= x 2))
              (vals (frequencies (map (fn [card] (rank card)) hand))))) 2)))

(defn straight? [hand]
  (let [high-ace (sort (map (fn [card] (rank card)) hand))
        low-ace (sort (replace {14 1} high-ace))
        same-range (fn [cards] (= cards (range (apply min cards) (+ (apply max cards) 1))))]
    (or (same-range low-ace) (same-range high-ace))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}
        valid-hands (filter (fn [[x _]] (x hand)) checkers)]
	(apply max (map (fn [[_ y]] y) valid-hands))))
