(ns p-p-p-pokerface)

(def rank-as-integer {\T 10, \J 11, \Q 12, \K 13, \A 14})

(defn rank [card]
  (let [[rank _] card]
    (if (Character/isDigit rank)
      (Integer/valueOf (str rank))
      (rank-as-integer rank))))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn ranks [hand]
  (map rank hand))

(defn ranks-by-amount [hand]
  (vals (frequencies (ranks hand))))

(defn ranks-sorted-by-amount [hand]
  (sort (ranks-by-amount hand)))

(defn max-number-of-same-rank [hand]
  (apply max (ranks-by-amount hand)))

(defn pair? [hand]
  (== (max-number-of-same-rank hand) 2))

(defn three-of-a-kind? [hand]
  (== (max-number-of-same-rank hand) 3))

(defn four-of-a-kind? [hand]
  (== (max-number-of-same-rank hand) 4))

(defn flush? [hand]
  (let [suits-by-amount (fn [] (vals (frequencies (map suit hand))))]
  (== (apply max (suits-by-amount)) 5)))

(defn full-house? [hand]
    (= [2 3] (ranks-sorted-by-amount hand)))

(defn two-pairs? [hand]
     (= [1 2 2] (ranks-sorted-by-amount hand)))

(defn straight? [hand]
  (let [sorted-orig (fn [] (sort (ranks hand)))
        straight-orig (fn [] (range (first (sorted-orig)) (+ (first (sorted-orig)) 5)))
        sorted-mod (fn [] (sort (replace {14 1} (ranks hand))))
        straight-mod (fn [] (range (first (sorted-mod)) (+ (first (sorted-mod)) 5)))]
  (or (= (straight-orig)
         (sorted-orig))
      (= (straight-mod)
         (sorted-mod)))))

(defn straight-flush? [hand]
  (and (flush? hand) (straight? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (cond
    (straight-flush? hand) 8
    (four-of-a-kind? hand) 7
    (full-house? hand) 6
    (flush? hand) 5
    (straight? hand) 4
    (three-of-a-kind? hand) 3
    (two-pairs? hand) 2
    (pair? hand) 1
    :else 0))
