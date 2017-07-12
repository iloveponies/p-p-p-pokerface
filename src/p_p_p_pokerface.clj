(ns p-p-p-pokerface)

(defn high-card? [hand]
  true)

(defn rank [card]
  (let [ranks {\T 10, \J 11, \Q 12, \K 13, \A 14}
        [value _] card]
    (if (Character/isDigit value)
      (Integer/valueOf (str value))
      (get ranks value))))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn pair? [hand]
  (let [ranks (map rank hand)]
    (< 1 (apply max (vals (frequencies ranks))))))

(defn three-of-a-kind? [hand]
  (let [ranks (map rank hand)]
    (< 2 (apply max (vals (frequencies ranks))))))

(defn four-of-a-kind? [hand]
  (let [ranks (map rank hand)]
    (< 3 (apply max (vals (frequencies ranks))))))

(defn flush? [hand]
  (let [suits (map suit hand)]
    (== 1 (count (vals (frequencies suits))))))

(defn full-house? [hand]
  (let [ranks (map rank hand)]
    (< 1 (apply min (vals (frequencies ranks))))))

(defn two-pairs? [hand]
  (let [freqs (vals (frequencies (map rank hand)))]
    (let [filtered (filter (fn [x] (<= 2 x)) freqs)]
      (cond
        (== 0 (count filtered)) false
        (== 2 (count filtered)) true
        (== 4 (apply max filtered)) true
        :else false))))

(defn straight? [hand]
  (let [ranks (map rank hand)]
    (let [min-value (apply min ranks)]
      (cond
        (== 14 (apply max ranks))
        (or (= (sort ranks) (range min-value (+ min-value 5)))
            (= (sort (replace {14 1} ranks)) (range 1 6)))
        :else (= (sort ranks) (range min-value (+ min-value 5)))))))

(defn straight-flush? [hand]
   (and (straight? hand) (flush? hand)))

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}]
    (apply max (map (fn [x] (if ((first x) hand) (second x) -1)) checkers))))
