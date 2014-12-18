(ns p-p-p-pokerface)

(def replacements {\T 10, \J 11, \Q 12, \K 13, \A 14})

(defn rank [card]
  (let [[rank _] card]
    (if (Character/isDigit rank) (Integer/valueOf (str rank)) (replacements rank))))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn n-of-a-kind? [hand n]
  (>= (apply max (vals (frequencies (map rank hand)))) n))

(defn pair? [hand]
  (n-of-a-kind? hand 2))

(defn three-of-a-kind? [hand]
  (n-of-a-kind? hand 3))

(defn four-of-a-kind? [hand]
  (n-of-a-kind? hand 4))

(defn flush? [hand]
  (apply = (map suit hand)))

(defn pair-points [hand]
  (let [seq (filter (fn [x] (> x 1)) (vals (frequencies (map rank hand))))]
    (reduce + seq)))

(defn full-house? [hand]
  (== (pair-points hand) 5))

(defn two-pairs? [hand]
  (>= (pair-points hand) 4))

(defn straight? [hand]
  (let [max-straight (sort (map rank hand))
        min-straight (sort (replace {14 1} max-straight))
        max-fst (first max-straight)]
    (or (= min-straight (range 1 6))
        (= max-straight (range max-fst (+ max-fst 5))))))

(defn straight-flush? [hand]
  (and (flush? hand) (straight? hand)))

(defn value [hand]
  (let [high-card? (fn [x] true)
        value (fn [x] ((first x) hand))
        checkers #{[high-card? 0] [pair? 1]
                   [two-pairs? 2] [three-of-a-kind? 3]
                   [straight? 4] [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}]
    (apply max (map second (filter value checkers)))))
