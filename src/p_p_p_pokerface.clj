(ns p-p-p-pokerface)

(defn rank [card]
  (let [ranks {\T 10, \J 11, \Q 12, \K 13, \A 14}
        [fst _] card]
    (if (Character/isDigit fst)
      (Integer/valueOf (str fst))
      (ranks fst))))

(defn freqs-of-ranks [hand]
  (let [ranks (map rank hand)]
    (vals (frequencies ranks))))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn pair? [hand]
  (contains? (set (freqs-of-ranks hand)) 2))

(defn three-of-a-kind? [hand]
    (contains? (set (freqs-of-ranks hand)) 3))

(defn four-of-a-kind? [hand]
    (contains? (set (freqs-of-ranks hand)) 4))

(defn flush? [hand]
  (let [suits (map suit hand)]
    (== 1 (count (set suits)))))

(defn full-house? [hand]
  (and (three-of-a-kind? hand) (pair? hand)))

(defn two-pairs? [hand]
  (let [two-pairs-freqs '(1 2 2)]
    (or (four-of-a-kind? hand)
        (= two-pairs-freqs (sort (freqs-of-ranks hand))))))

(defn straight? [hand]
  (let [sorted-ranks-high (sort (map rank hand))
        sorted-ranks-low (sort (replace {14 1} sorted-ranks-high))
        straight-ranks? (fn [x] (= x (range (first x) (+ 5 (first x)))))]
    (or (straight-ranks? sorted-ranks-high)
        (straight-ranks? sorted-ranks-low))))

(defn straight-flush? [hand]
  (and (straight? hand)
       (flush? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}
        has-value? (fn [checker] ((first checker) hand))
        get-values (map second (filter has-value? checkers))]
    (apply max get-values)))

