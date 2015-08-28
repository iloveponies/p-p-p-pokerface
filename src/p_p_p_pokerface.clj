(ns p-p-p-pokerface)

(defn rank [card]
  (let [[x _] card]
    (cond
      (Character/isDigit x) (Integer/valueOf (str x))
      :else (get {\T 10, \J 11, \Q 12, \K 13, \A 14} x))))

(defn suit [card]
  (let [[_ x] card]
    (str x)))

(defn n-of-a-kind? [hand n]
  (< (- n 1) (apply max (vals (frequencies (map rank hand))))))
  
(defn pair? [hand]
  (n-of-a-kind? hand 2))

(defn three-of-a-kind? [hand]
  (n-of-a-kind? hand 3))

(defn four-of-a-kind? [hand]
  (n-of-a-kind? hand 4))

(defn flush? [hand]
  (= 5 (apply max (vals (frequencies (map suit hand))))))

(defn full-house? [hand]
  (= [2 3] (sort (vals (frequencies (map rank hand))))))

(defn two-pairs? [hand]
  (or
    (= [1 2 2] (sort (vals (frequencies (map rank hand)))))
    (four-of-a-kind? hand)
    (full-house? hand)))

(defn straight? [hand]
  (let [ranks (sort (map rank hand))]
    (if (= [2 3 4 5 14] ranks)
      true
      (= ranks (range (first ranks) (+ 1 (last ranks)))))))

(defn straight-flush? [hand]
  (and
    (straight? hand)
    (flush? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                   [two-pairs? 2]  [three-of-a-kind? 3]
                   [straight? 4]   [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
		   [straight-flush? 8]}]
    (let [matches (filter (fn [x] ((first x) hand)) checkers)]
      (let [match-values (map second matches)]
        (apply max match-values)))))
                 