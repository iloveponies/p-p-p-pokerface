(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rank _] card]
    (if (Character/isDigit rank)
      (Integer/valueOf (str rank))
      ({\T 10, \J 11, \Q 12, \K 13, \A 14} rank))))

(defn suit [card]
  (let [[_ suit] card] (str suit)))

(defn pair? [hand]
  (>= (apply max (vals (frequencies (map rank hand)))) 2))

(defn three-of-a-kind? [hand]
  (== (apply max (vals (frequencies (map rank hand)))) 3))

(defn four-of-a-kind? [hand]
  (== (apply max (vals (frequencies (map rank hand)))) 4))

(defn flush? [hand]
  (apply = (map suit hand)))

(defn full-house? [hand]
  ( = (sort (vals (frequencies (map rank hand)))) '(2 3)))

(defn two-pairs? [hand]
  ( = (sort (vals (frequencies (map rank hand)))) '(1 2 2)))

(defn straight? [hand]
  (let [sorted (sort (map rank hand))]
    (cond
      (= sorted (range (apply min sorted) (+ (apply min sorted) 5))) true
      (= (sort (replace {14 1} sorted)) (range 1 6)) true
      :else false)))

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
    (apply max (remove nil? (map (fn [value-pair] (let [[func value] value-pair] (if (func hand) value ))) checkers)))))
