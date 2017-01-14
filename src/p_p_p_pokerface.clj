(ns p-p-p-pokerface)

(def upper-ranks {\T 10, \J 11, \Q 12, \K 13, \A 14})

(defn rank [[r]]
  (if (Character/isDigit r)
    (Integer/valueOf (str r))
    (upper-ranks r)))

(defn suit [[_ s]]
  (str s))

(defn n-of-a-kind? [n hand]
  (not (empty? (filter (fn [x] (== x n)) (vals (frequencies (map rank hand)))))))

(defn pair? [hand]
  (n-of-a-kind? 2 hand))

(defn three-of-a-kind? [hand]
  (n-of-a-kind? 3 hand))

(defn four-of-a-kind? [hand]
  (n-of-a-kind? 4 hand))

(defn flush? [hand]
  (== (count (frequencies (map suit hand))) 1))

(defn full-house? [hand]
  (= (sort (vals (frequencies (map rank hand)))) (range 2 4)))

(defn two-pairs? [hand]
  (= ((frequencies (vals (frequencies (map rank hand)))) 2) 2))

(defn straight? [hand]
  (let [ranks (map rank hand)
        rep-ranks (replace {14 1} ranks)
        consec (fn [h]
                 (let [sorted (sort h)
                       smallest (first sorted)
                       biggest (+ smallest (count sorted))]
                   (= sorted (range smallest biggest))))]
    (or (consec ranks) (consec rep-ranks))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [_] true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                   [two-pairs? 2]  [three-of-a-kind? 3]
                   [straight? 4]   [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}]
    (apply max (map (fn [[f v]] (if (f hand) v 0)) checkers))))
