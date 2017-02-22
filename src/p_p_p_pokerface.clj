(ns p-p-p-pokerface)

(def replacements {\T 10,
                   \J 11,
                   \Q 12,
                   \K 13,
                   \A 14})

(defn rank [card]
  (let [[rank _] card]
    (cond
      (Character/isDigit rank) (Integer/valueOf (str rank))
      :else (replacements rank))))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn rank-frequency-values [hand]
  (set (vals (frequencies (map rank hand)))))

(defn pair? [hand]
  (contains? (rank-frequency-values hand) 2))

(defn three-of-a-kind? [hand]
  (contains? (rank-frequency-values hand) 3))

(defn four-of-a-kind? [hand]
  (contains? (rank-frequency-values hand) 4))

(defn flush? [hand]
  (contains? (set( vals (frequencies (map suit hand)))) 5))

(defn full-house? [hand]
  (and
    (three-of-a-kind? hand)
    (pair? hand)))

(defn two-pairs? [hand]
  (let [freq (frequencies (vals (frequencies (map rank hand))))]
    (or (= 2 (get freq 2)) (= 1 (get freq 4)))))

(defn straight? [hand]
  (let [sorted (sort (map rank hand))
        sorted-low (sort (replace {14 1} sorted))]
    (or
      (= sorted (range (first sorted) (+ 1 (last sorted))))
      (= sorted-low (range (first sorted-low) (+ 1 (last sorted-low)))))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  (or
    (= 5 (count (frequencies (map rank hand))))
    (not (straight? hand))))

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                   [two-pairs? 2]  [three-of-a-kind? 3]
                   [straight? 4]   [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}
        apply-matcher
          (fn [pair] (if ((first pair) hand)
                      (second pair)
                      0))]
    (apply max (map apply-matcher checkers))))
