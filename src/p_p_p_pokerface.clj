(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rank _] card]
    (if (Character/isDigit rank)
      (Integer/valueOf (str rank))
      (get {\T 10, \J 11, \Q 12, \K 13, \A 14} rank))))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn n-of-a-kind? [n hand]
  (contains? (set (vals (frequencies (map rank hand)))) n))

(defn pair? [hand]
  (n-of-a-kind? 2 hand))

(defn three-of-a-kind? [hand]
  (n-of-a-kind? 3 hand))

(defn four-of-a-kind? [hand]
  (n-of-a-kind? 4 hand))

(defn flush? [hand]
  (= (count (set (map suit hand))) 1))

(defn full-house? [hand]
  (and (n-of-a-kind? 2 hand)
       (n-of-a-kind? 3 hand)))

(defn two-pairs? [hand]
  (and (<= 2 (count (set (map rank hand))) 3)
       (not (three-of-a-kind? hand))))

(defn straight? [hand]
  (let [f (fn [r] (and (apply < (sort r))
                       (== (- (apply max r) (apply min r)) 4)))
        ranks (map rank hand)]
    (or (f ranks) (f (replace {14 1} ranks)))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn value [hand]
  (cond (straight-flush?  hand) 8
        (four-of-a-kind?  hand) 7
        (full-house?      hand) 6
        (flush?           hand) 5
        (straight?        hand) 4
        (three-of-a-kind? hand) 3
        (two-pairs?       hand) 2
        (pair?            hand) 1
        :else                   0))
