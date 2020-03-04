(ns p-p-p-pokerface)

(def facecards {\T 10,
                \J 11,
                \Q 12,
                \K 13,
                \A 14})


(defn rank [card]
  (let [[r _] card]
    (if (Character/isDigit r)
      (Integer/valueOf (str r))
      (Integer/valueOf (get facecards r)))))

(defn high-card? [hand]
  true) ; All hands have a high card.

(defn suit [card]
  (let [[_ s] card]
    (str s)))

(defn rank-frequencies [hand]
  (vals (frequencies (map rank hand))))

(defn suit-frequencies [hand]
  (vals (frequencies (map suit hand))))

(defn pair? [hand]
  (== 2 (apply max (rank-frequencies hand))))

(defn three-of-a-kind? [hand]
  (== 3 (apply max (rank-frequencies hand))))

(defn four-of-a-kind? [hand]
  (== 4 (apply max (rank-frequencies hand))))

(defn flush? [hand]
  (== 5 (apply max (suit-frequencies hand))))

(defn full-house? [hand]
  (let [val-freqs (sort (rank-frequencies hand))]
    (and (== 2 (first val-freqs))
         (== 3 (second val-freqs)))))

(defn two-pairs? [hand]
  (let [val-freqs (rank-frequencies hand)]
    (or (== 2 (first val-freqs) (second val-freqs))
        (four-of-a-kind? hand))))

(defn straight? [hand]
  (let [num-hand (sort (replace facecards (map rank hand)))
        max-rank (apply max num-hand)
        min-rank (apply min num-hand)]
    (cond
      (and (== max-rank 14) (== min-rank 10)) (= num-hand (range 10 15))
      (and (== max-rank 14) (== min-rank 2)) (= num-hand (seq [2 3 4 5 14]))
      :else (= num-hand (range min-rank (+ 1 max-rank))))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                   [two-pairs? 2]  [three-of-a-kind? 3]
                   [straight? 4]   [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}
        check-one (fn [[hand? _]] (hand? hand))]
  (apply max (map second (filter check-one checkers)))))
