(ns p-p-p-pokerface)

(defn rank [card]
  (let [replacements {\T 10, \J 11, \Q 12, \K 13, \A 14}
        [r _] card]
    (if (Character/isDigit r)
      (Integer/valueOf (str r))
      (Integer/valueOf (replacements r)))))


(defn suit [card]
  (let [[_ s] card]
    (str s)))

(defn pair? [hand]
  (> (apply max (vals (frequencies (map rank hand)))) 1))

(defn three-of-a-kind? [hand]
  (> (apply max (vals (frequencies (map rank hand)))) 2))

(defn four-of-a-kind? [hand]
  (> (apply max (vals (frequencies (map rank hand)))) 3))

(defn flush? [hand]
  (== (count (vals (frequencies (map suit hand)))) 1))

(defn full-house? [hand]
  (= (range 2 4) (sort (vals (frequencies (map rank hand))))))

(defn two-pairs? [hand]
  (= (seq [1 2 2]) (sort (vals (frequencies (map rank hand))))))

(defn straight? [hand]
  (let [order (sort (map rank hand))
        alt-order (sort (replace {14 1} order))]
    (or (= order (range (first order) (+ (first order) 5)))
        (= alt-order (range (first alt-order) (+ (first alt-order) 5))))))


(defn straight-flush? [hand]
  (and (flush? hand) (straight? hand)))

(defn value [hand]
  (let [high-card? (fn [h] true)
        checkers #{[high-card? 0]  [pair? 1]
                   [two-pairs? 2]  [three-of-a-kind? 3]
                   [straight? 4]   [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}
        evaluator (fn [checker] ((first checker) hand))]
    (first (reverse (sort
                     (map second
                          (filter evaluator checkers)))))))
