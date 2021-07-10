(ns p-p-p-pokerface)

(defn rank [card]
  (let [[r _] card
        monkey-to-int {\A 14,
                       \K 13,
                       \Q 12,
                       \J 11,
                       \T 10}]
    (if (Character/isDigit r)
      (Integer/valueOf (str r))
      (monkey-to-int r))))

(defn suit [card]
  (let [[_ s] card]
    (str s)))

(defn pair? [hand]
  (> (apply max (vals (frequencies (map rank hand)))) 1))

(defn three-of-a-kind? [hand]
  (> (apply max (vals (frequencies (map rank hand)))) 2))

(defn four-of-a-kind? [hand]
  (> (apply max (vals (frequencies (map rank hand)))) 3))

(defn flush? [hand]
  (= (apply max (vals (frequencies (map suit hand)))) 5))

(defn full-house? [hand]
  (let [vs (set (vals (frequencies (map rank hand))))]
    (and (contains? vs 2) (contains? vs 3))))

(defn two-pairs? [hand]
  (let [vs (frequencies (vals (frequencies (map rank hand))))]
    (or (= (get vs 2 0) 2) (= (get vs 4 0) 1))))

(defn straight? [hand]
  (let [rh14 (sort (map rank hand))
        rh14_min (apply min rh14)
        rh1 (sort (replace {14 1} (map rank hand)))
        rh1_min (apply min rh1)]
    (or (= (range rh1_min (+ rh1_min (count hand))) rh1)
        (= (range rh14_min (+ rh14_min (count hand))) rh14))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn value [hand]
  (let [checkers #{[pair? 1]
                   [two-pairs? 2]
                   [three-of-a-kind? 3]
                   [straight? 4]
                   [flush? 5]
                   [full-house? 6]
                   [four-of-a-kind? 7]
                   [straight-flush? 8]}]
    (apply max (map (fn [[c v]] (if (c hand) v 0)) checkers))))
