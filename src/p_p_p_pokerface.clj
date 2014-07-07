(ns p-p-p-pokerface)

(defn rank [[rank _]]
  (let [ranks {\T 10, \J 11, \Q 12, \K 13, \A 14}]
    (cond
      (Character/isDigit rank) (Integer/valueOf (str rank))
      :else (get ranks rank)
  )))

(defn suit [[_ suit]]
  (str suit))

(defn pair? [hand]
  (>= (apply max (vals (frequencies (map rank hand)))) 2))

(defn three-of-a-kind? [hand]
  (>= (apply max (vals (frequencies (map rank hand)))) 3))

(defn four-of-a-kind? [hand]
  (>= (apply max (vals (frequencies (map rank hand)))) 4))

(defn flush? [hand]
  (= 1 (count (vals (frequencies (map suit hand))))))

(defn full-house? [hand]
  (let [freq (vals (frequencies (map rank hand)))]
    (and (= 3 (apply max freq))
      (= 2 (apply min freq)))))

(defn two-pairs? [hand]
  (let [freq (vals (frequencies (map rank hand)))]
    (and (= 2 (apply max freq))
      (= 3 (count freq)))))

(defn straight? [hand]
  (let [ranks (sort (map rank hand))
        diff (- (apply max ranks) (apply min ranks))]
    (or (= ranks (range (apply min ranks) (+ 1 (apply max ranks))))
      (= ranks [2 3 4 5 14]))))

(defn straight-flush? [hand]
  nil)

(defn value [hand]
  nil)
