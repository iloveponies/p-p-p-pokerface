(ns p-p-p-pokerface)

(defn rank [card]
  (let [[r _] card]
    (let [ranks {\T 10, \J 11 \Q 12, \K 13, \A 14}]
      (if (Character/isDigit r) (Integer/valueOf (str r)) (get ranks r)))))

(defn suit [card]
  (let [[_ s] card]
    (str s)))

(defn pair? [hand]
  (= 4 (count (frequencies (map rank hand)))))

(defn three-of-a-kind? [hand]
  (= 3 (apply max (vals (frequencies (map rank hand))))))

(defn four-of-a-kind? [hand]
  (= 4 (apply max (vals (frequencies (map rank hand))))))

(defn flush? [hand]
  (= 1 (count (frequencies (map suit hand)))))

(defn full-house? [hand]
  (let [freqs (frequencies (map rank hand))]
    (and
      (= 3 (apply max (vals freqs)))
      (= 2 (count freqs)))))

(defn two-pairs? [hand]
  (= 2 (count (filter
    (fn [rank] (= 2 rank))
    (vals (frequencies (map rank hand)))))))

(defn straight? [hand]
  (let [ranks (map rank hand)]
    (let [sorted-hand-ace-high (sort ranks)
          sorted-hand-ace-low (sort (replace {14 1} ranks))]
      (let [min-rank (first sorted-hand-ace-high)]
        (or (= sorted-hand-ace-low (range 1 6))
            (= sorted-hand-ace-high (range min-rank (+ min-rank 5))))))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                   [two-pairs? 2]  [three-of-a-kind? 3]
                   [straight? 4]   [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}]
    (apply max (map (fn [checker] (second checker))
      (filter (fn [checker] ((first checker) hand)) checkers)))))
