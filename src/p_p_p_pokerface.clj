(ns p-p-p-pokerface)

(defn hand [a-hand]
  a-hand)

(defn card [a-card]
  a-card)

(defn rank [card]
  (let [[card-rank _] card]
    (if (Character/isDigit card-rank)
        (Integer/valueOf (str card-rank))
        ({\T 10, \J 11, \Q 12, \K 13, \A 14} card-rank))))

(defn suit [card]
  (let [[_ card-suit] card]
    (str card-suit)))

(defn pair? [hand]
  (> (apply max (vals (frequencies (map rank hand)))) 1))

(defn three-of-a-kind? [hand]
  (> (apply max (vals (frequencies (map rank hand)))) 2))

(defn four-of-a-kind? [hand]
  (> (apply max (vals (frequencies (map rank hand)))) 3))

(defn flush? [hand]
  (== (apply max (vals (frequencies (map suit hand)))) 5))

(defn full-house? [hand]
  (= (sort (vals (frequencies(map rank hand)))) [2 3]))

(defn two-pairs? [hand]
  (let [rank-dist (sort (vals (frequencies(map rank hand))))]
    (or (= rank-dist [1 2 2]) (= rank-dist [1 4]))))

(defn straight? [hand]
  (let [rank-dist (sort (map rank hand)),
        low-ace-rank-dist (sort (replace {14 1} rank-dist)),
        rank-min (apply min rank-dist),
        low-ace-rank-min (apply min low-ace-rank-dist)]
    (or (= rank-dist (range rank-min (+ rank-min 5)))
        (= low-ace-rank-dist (range low-ace-rank-min (+ low-ace-rank-min 5))))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  true) ; All hands have a high card.

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                   [two-pairs? 2]  [three-of-a-kind? 3]
                   [straight? 4]   [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]},
        pair-first (fn[pair] ((first pair) hand))]
    (apply max (map second (filter pair-first checkers)))))

