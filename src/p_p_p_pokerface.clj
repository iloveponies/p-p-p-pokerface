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

; TODO: A can be 1 or 14
(defn straight? [hand]
  (let [sorted-hand (sort (map rank hand))]
    (let [min-rank (first sorted-hand)]
      (= sorted-hand (range min-rank (+ min-rank 5))))))

(defn straight-flush? [hand]
  nil)

(defn value [hand]
  nil)
