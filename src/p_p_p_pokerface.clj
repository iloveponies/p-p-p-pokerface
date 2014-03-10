(ns p-p-p-pokerface)

(defn rank [card]
  (let [[value _] card
        char-values {\T 10, \J 11, \Q 12, \K 13, \A 14}]
    (if (Character/isDigit value) 
      (Integer/valueOf (str value))
      (get char-values value))))

(defn suit [card]
  (let [[_ s] card]
    (str s)))

(defn pair? [hand]
  (= (apply max (vals (frequencies (map rank hand)))) 2))

(defn three-of-a-kind? [hand]
  (= (apply max (vals (frequencies (map rank hand)))) 3))

(defn four-of-a-kind? [hand]
  (= (apply max (vals (frequencies (map rank hand)))) 4))

(defn flush? [hand]
  (= (first (vals (frequencies (map suit hand)))) 5))

(defn full-house? [hand]
  (= '(2 3) (sort (vals (frequencies (map rank hand))))))

(defn two-pairs? [hand]
  (= (get (frequencies (vals (frequencies (map rank hand)))) 2) 2))

(defn straight? [hand]
  (let [sorted-ranks (sort (map rank hand))
        last-rank (last sorted-ranks)]
    (or
      (= sorted-ranks (range (first sorted-ranks) (+ last-rank 1)))
      (and (= (range 2 6) (take 4 sorted-ranks))
           (= last-rank 14)))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn value [hand]
  (let [high-card? (fn [_] true)
        checkers #{[high-card? 0] [pair? 1]
                   [two-pairs? 2] [three-of-a-kind? 3]
                   [straight? 4] [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}]
    (apply max (map last (filter (fn [tuple] (let [[function value] tuple] (function hand))) checkers)))))
