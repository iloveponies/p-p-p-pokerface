(ns p-p-p-pokerface)

(defn rank [card]
  (let [[fst _] card]
    (def big-guns {\T 10, \J 11, \Q 12, \K 13, \A 14})
    (if (Character/isDigit fst) (Integer/valueOf (str fst)) (big-guns fst))))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn hand-ranks [hand]
  (let [[c1 c2 c3 c4 c5] hand
        hand [(rank c1) (rank c2) (rank c3) (rank c4) (rank c5)]]
    hand))

(defn hand-suits [hand]
  (let [[c1 c2 c3 c4 c5] hand
        hand [(suit c1) (suit c2) (suit c3) (suit c4) (suit c5)]]
    hand))

(defn hand-rank-freqs [hand]
  (vals (frequencies (hand-ranks hand))))

(defn hand-suits-freqs [hand]
  (vals (frequencies (hand-suits hand))))

(defn pair? [hand]
  (== 2 (apply max (hand-rank-freqs hand))))

(defn three-of-a-kind? [hand]
  (== 3 (apply max (hand-rank-freqs hand))))

(defn four-of-a-kind? [hand]
  (== 4 (apply max (hand-rank-freqs hand))))

(defn flush? [hand]
  (== 5 (apply max (hand-suits-freqs hand))))

(defn full-house? [hand]
  (let [freqs (hand-rank-freqs hand)]
    (and (== 2 (count freqs)) (== 3 (apply max freqs)))))

(defn two-pairs? [hand]
  (let [freqs (hand-rank-freqs hand)]
    (or (== 4 (apply max freqs)) (and (== 3 (count freqs)) (== 2 (apply max freqs))))))

(defn straight? [hand]
  (let [hand (sort (hand-ranks hand))
        hand-ace-one (sort (replace {14 1} hand))]
    (defn str? [cards]
      (= cards (range (first cards) (+ (first cards) 5))))
    (or (str? hand) (str? hand-ace-one))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn value [hand]
  (defn high-card? [hand] true)
  (let [checkers #{[high-card? 0] [pair? 1] [two-pairs? 2] [three-of-a-kind? 3] [straight? 4] [flush? 5] [full-house? 6] [four-of-a-kind? 7][straight-flush? 8]}
        filtered-hands (filter (fn [x] ((first x) hand)) checkers)
        filtered-hand-values (map #(apply (fn [_ n] n) %) filtered-hands)]
    (apply max filtered-hand-values)))
