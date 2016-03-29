(ns p-p-p-pokerface)

(def card-map {\T 10,
               \J 11,
               \Q 12
               \K 13
               \A 14})

(defn rank [card]
  (let [[fst _] card]
    (if (Character/isDigit fst)
      (Integer/valueOf (str fst))
      (card-map fst))))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn ranks [hand]
  (vals (frequencies (map rank hand))))

(defn max-same-rank [hand]
  (apply max (ranks hand)))

(defn max-same-suit [hand]
  (apply max (vals (frequencies (map suit hand)))))

(defn high-card? [hand]
  true)

(defn pair? [hand]
  (>= (max-same-rank hand) 2))

(defn three-of-a-kind? [hand]
  (>= (max-same-rank hand) 3))

(defn four-of-a-kind? [hand]
  (>= (max-same-rank hand) 4))

(defn flush? [hand]
  (= (max-same-suit hand) 5))

(defn full-house? [hand]
  (let [rnks (sort (ranks hand))]
    (= rnks [2 3])))

(defn two-pairs? [hand]
  (let [four (four-of-a-kind? hand)
        pairs (= (sort (ranks hand)) [1 2 2])]
    (or four pairs)))

(defn straight? [hand]
  (let [straight-helper? (fn [ranks] (let [min-rank (apply min ranks)
                                           max-rank (apply max ranks)]
                                       (= ranks (range min-rank (+ 1 max-rank)))))
        high-ace (sort (map rank hand))
        low-ace (sort (replace {14 1} (map rank hand)))]
    (or (straight-helper? high-ace) (straight-helper? low-ace))))

(defn straight-flush? [hand]
  (and (flush? hand) (straight? hand)))

(defn value [hand]
  (let [checkers #{[high-card? 0] [pair? 1] [two-pairs? 2]
                   [three-of-a-kind? 3] [straight? 4] [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7] [straight-flush? 8]}
        hand-value (fn [[fnc val]] (if (fnc hand) val 0))]
    (apply max (map hand-value checkers))))
