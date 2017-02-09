

(ns p-p-p-pokerface)

(defn rank [card]
  (def replacements {\T 10, \J 11, \Q 12, \K 13, \A 14})
  (let [[rank _] card]
    (if (Character/isDigit rank)
      (Integer/valueOf (str rank))
      (replacements rank))))

(defn rank-counts [hand]
  (vals (frequencies (map rank hand))))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn pair? [hand]
  (< 1 (apply max (rank-counts hand))))

(defn three-of-a-kind? [hand]
  (< 2 (apply max (rank-counts hand))))

(defn four-of-a-kind? [hand]
  (< 3 (apply max (rank-counts hand))))

(defn flush? [hand]
  (= 5 (apply max (vals (frequencies (map suit hand))))))

(defn full-house? [hand]
  (= '(2 3) (sort (rank-counts hand))))

(defn two-pairs? [hand]
  (or (= '(1 2 2) (sort (rank-counts hand)))
      (four-of-a-kind? hand)))

(defn consecutive? [sequence]
  (let [max-value (apply max sequence)
        min-value (apply min sequence)]
    (= (sort sequence) (range min-value (+ 1 max-value)))))

(defn straight? [hand]
  (let [ranks (map rank hand)]
    (or (consecutive? ranks)
        (consecutive? (replace {14 1} ranks)))))


(defn straight-flush? [hand]
  (and (flush? hand) (straight? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}]
    (apply max
           (map second
                (filter (fn [checker] ((first checker) hand)) checkers)))))
