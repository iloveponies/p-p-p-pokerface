(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rank suit] card
        replacements {\T 10, \J 11, \Q 12, \K 13, \A 14}]
    (if (Character/isDigit rank) (Integer/valueOf (str rank)) (replacements rank))))

(defn suit [card]
  (let [[rank suit] card]
    (str suit)))

(defn rank-frequencies [hand]
  (vals (frequencies (map rank hand))))

(defn pair? [hand]
  (< 1 (apply max (rank-frequencies hand))))

(defn three-of-a-kind? [hand]
  (< 2 (apply max (rank-frequencies hand))))

(defn four-of-a-kind? [hand]
  (< 3 (apply max (rank-frequencies hand))))

(defn flush? [hand]
  (< 4 (apply max (vals (frequencies (map suit hand))))))

(defn full-house? [hand]
  (let [freqs (sort (rank-frequencies hand))]
    (= freqs (range 2 4))))

(defn two-pairs? [hand]
  (let [freqs (frequencies (rank-frequencies hand))]
    (or (= (get freqs 2) 2) (= (get freqs 4) 1))))

(defn straight? [hand]
  (let [vals (map rank hand)
        ace-one-vals (replace {14 1} (map rank hand))]
    (and (or
           (= (count (range (apply min vals) (+ 1 (apply max vals)))) 5)
           (= (count (range (apply min ace-one-vals) (+ 1 (apply max ace-one-vals)))) 5))
         (not (pair? hand)))))

(defn straight-flush? [hand]
  (and (flush? hand) (straight? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (apply max
    (let [checkers #{[high-card? 0]  [pair? 1]
                     [two-pairs? 2]  [three-of-a-kind? 3]
                     [straight? 4]   [flush? 5]
                     [full-house? 6] [four-of-a-kind? 7]
                     [straight-flush? 8]}]
      (filter boolean
        (map (fn [checker] (let [[func score] checker] (if (func hand) score))) checkers)))))


