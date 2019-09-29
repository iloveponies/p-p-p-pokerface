(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rank _] card]
    (if (Character/isDigit rank)
        (Integer/valueOf (str rank))
        (get {\T 10, \J 11, \Q 12, \K 13, \A 14} rank))))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn frequence-vals [hand]
  (vals (frequencies (map rank hand))))

(defn max-same-ranks [hand]
  (apply max (frequence-vals hand)))

(defn pair? [hand]
  (<= 2 (max-same-ranks hand)))

(defn three-of-a-kind? [hand]
  (<= 3 (max-same-ranks hand)))

(defn four-of-a-kind? [hand]
  (<= 4 (max-same-ranks hand)))

(defn flush? [hand]
  (== 1 (count (keys (frequencies (map suit hand))))))

(defn full-house? [hand]
  (= '(2 3) (sort (frequence-vals hand))))

(defn two-pairs? [hand]
  (or
    (= '(1 2 2) (sort (frequence-vals hand)))
    (= '(1 4) (sort (frequence-vals hand)))))

(defn straight? [hand]
  (let [in-order (sort (map rank hand))
        first-in-order (first in-order)
        in-order-low-ace (sort(replace {14 1} in-order))
        first-low-ace (first in-order-low-ace)]
    (or
      (= in-order (range first-in-order (+ first-in-order 5)))
      (= in-order-low-ace (range first-low-ace (+ first-low-ace 5))))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0] [pair? 1] [two-pairs? 2] [three-of-a-kind? 3]
                   [straight? 4] [flush? 5] [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}]
    (apply max (map second (filter (fn [[func points]] (func hand)) checkers)))))
