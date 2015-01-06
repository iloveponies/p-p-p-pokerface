(ns p-p-p-pokerface)

(defn rank [card]
    (let [[fst _] card
          replacements {\T 10, \J 11, \Q 12, \K 13, \A 14}]
        (if (Character/isDigit fst)
            (Integer/valueOf (str fst))
            (replacements fst))))

(defn suit [card]
    (let [[_ snd] card]
        (str snd)))

(defn pair? [hand]
    (>= (apply max (vals (frequencies (map rank hand)))) 2))

(defn three-of-a-kind? [hand]
    (>= (apply max (vals (frequencies (map rank hand)))) 3))

(defn four-of-a-kind? [hand]
    (>= (apply max (vals (frequencies (map rank hand)))) 4))

(defn flush? [hand]
    (apply = (map suit hand)))

(defn full-house? [hand]
    (= (sort (vals (frequencies (map rank hand)))) [2 3]))

(defn two-pairs? [hand]
    (or
        (= (sort (vals (frequencies (map rank hand)))) [1 2 2])
        (four-of-a-kind? hand)))

(defn straight? [hand]
    (let [v1 (sort (map rank hand))
          v2 (sort (replace {14 1} v1))
          m1 (apply min v1)
          m2 (apply min v2)
          cmp1 (range m1 (+ m1 5))
          cmp2 (range m2 (+ m2 5))]
        (or (= v1 cmp1) (= v2 cmp2))))

(defn straight-flush? [hand]
    (and (straight? hand) (flush? hand)))

(defn value [hand]
    (let [high-card? (fn [hand] true)
          checkers #{[high-card? 0] [pair? 1] [two-pairs? 2] [three-of-a-kind? 3] [straight? 4]
                     [flush? 5] [full-house? 6] [four-of-a-kind? 7] [straight-flush? 8]}]
        (apply max (map second (filter (fn [x] ((first x) hand)) checkers)))))
