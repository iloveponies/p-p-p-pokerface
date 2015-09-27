(ns p-p-p-pokerface)

(defn rank [card]
  (let [ranks {\T 10, \J 11, \Q 12, \K 13, \A 14}
        [s-rank _] card]
    (if (Character/isDigit s-rank)
      (Integer/valueOf (str s-rank))
      (ranks s-rank))))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn pair? [hand]
  (if (contains? (set (vals (frequencies (map rank hand)))) 2)
    true
    false))

(defn three-of-a-kind? [hand]
  (if (contains? (set (vals (frequencies (map rank hand)))) 3)
    true
    false))

(defn four-of-a-kind? [hand]
  (if (contains? (set (vals (frequencies (map rank hand)))) 4)
    true
    false))

(defn flush? [hand]
  (if (= 5 (first (vals (frequencies (map suit hand)))))
    true
    false))

(defn full-house? [hand]
  (and (pair? hand) (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  (if (four-of-a-kind? hand)
    true
    (if (contains? (set (frequencies (vals (frequencies (map rank hand)))))
                   [2 2])
      true
      false)))

(defn straight? [hand]
  (let [ranks-set     (set (map rank hand))
        test-straight (fn [set]
                        (let [sorted       (sort set)
                              first-sorted (first sorted)]
                          (= sorted (range first-sorted (+ first-sorted 5)))))]
    (if (contains? ranks-set 14)
      (or
        (test-straight ranks-set)
        (test-straight (replace {14 1} ranks-set)))
      (test-straight ranks-set))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}
        checkers-filter (fn [[func val]]
                          (if (func hand)
                            val
                            0))]
    (apply max (map checkers-filter checkers))))
