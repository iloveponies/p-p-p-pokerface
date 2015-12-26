(ns p-p-p-pokerface)

(defn rank [card]
  (let [[fst _] card]
    (if (Character/isDigit fst)
      (Integer/valueOf (str fst))
      ({\T 10,
        \J 11,
        \Q 12,
        \K 13,
        \A 14} fst))))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn pair? [hand]
  (> (apply max (vals (frequencies (map rank hand))))
     1))

(defn three-of-a-kind? [hand]
  (> (apply max (vals (frequencies (map rank hand))))
     2))

(defn four-of-a-kind? [hand]
  (> (apply max (vals (frequencies (map rank hand))))
     3))

(defn flush? [hand]
  (= (apply max (vals (frequencies (map suit hand))))
     5))

(defn full-house? [hand]
  (= (take-last 2 (sort (vals (frequencies (map rank hand)))))
     '(2 3)))

(defn two-pairs? [hand]
  (or (four-of-a-kind? hand)
      (= (take-last 2 (sort (vals (frequencies (map rank
                                                    hand)))))
         '(2 2))))

(defn straight? [hand]
  (let [hand-vals (map (fn [x] (- (rank x) 1)) hand)
        hand-min (apply min hand-vals)
        hand-mod (sort (map (fn [x]
                              (mod (+ x (- 13 hand-min)) 13))
                  hand-vals))]
    (or (= hand-mod '(0 1 2 3 4))
        (= hand-mod '(0 1 2 3 12))))) ; ugly catching of edge case :(

(defn straight-flush? [hand]
  (and (flush? hand)
       (straight? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                   [two-pairs? 2]  [three-of-a-kind? 3]
                   [straight? 4]   [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}]
    (apply max (map (fn [x]
                      (let [[func pts] x]
                        (if (func hand)
                          pts
                          0)))
                    checkers))))

