(ns p-p-p-pokerface)

(defn rank [card]
  (let [[val _] card]
    (if (Character/isDigit val)
      (Character/getNumericValue val)
      ({\T 10, \J 11, \Q 12, \K 13, \A 14} val))))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

;These four can use (some) instead of (contains (set)
;but value look ups should be done with sets/maps
(defn hand-freq-set [hand]
  (set (vals (frequencies (map rank hand)))))

(defn pair? [hand]
  (contains? (hand-freq-set hand) 2))

(defn three-of-a-kind? [hand]
  (contains? (hand-freq-set hand) 3))

(defn four-of-a-kind? [hand]
  (contains? (hand-freq-set hand) 4))

(defn flush? [hand]
  (= 1 (count (set (map suit hand)))))

(defn full-house? [hand]
  (and (pair? hand) (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  (= 2 (count (filter #{2} (vals (frequencies (map rank hand)))))))

(defn straight? [hand]
  (let [ranks-ha (sort (map rank hand))
        ranks-la (sort (replace {14 1} ranks-ha))
        comp #(= % (range (apply min %) (+ 1 (apply max %))))]
    (or (comp ranks-ha) (comp ranks-la))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand] true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                   [two-pairs? 2]  [three-of-a-kind? 3]
                   [straight? 4]   [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}]
    (let [true-checkers (filter (fn [[check _]] (check hand)) checkers)]
      (apply max (map second true-checkers)))))
