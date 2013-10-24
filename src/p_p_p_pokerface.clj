(ns p-p-p-pokerface)

(defn rank [card]
  (let [[value _] card
        values {\T 10, \J 11, \Q 12, \K 13, \A 14}
        ]
    (if (Character/isDigit value)
      (Integer/valueOf (str value))
      (values value))))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn freqs-of-ranks [hand]
  (let [ranks (map rank hand)
        freqs (vals (frequencies ranks))]
      (sort freqs)))

(defn most-common-rank [hand]
    (apply max (freqs-of-ranks hand)))

(defn pair? [hand]
  (>= (most-common-rank hand) 2))

(defn three-of-a-kind? [hand]
  (>= (most-common-rank hand) 3))

(defn four-of-a-kind? [hand]
  (>= (most-common-rank hand) 4))

(defn flush? [hand]
  (let [suits (map suit hand)
        freqs (vals (frequencies suits))
        most-common-suit (apply max freqs)]
    (== most-common-suit 5)))

(defn full-house? [hand]
  (= [2 3] (freqs-of-ranks hand)))

(defn two-pairs? [hand]
  (cond (four-of-a-kind? hand) true
        (full-house? hand) true
        (= [1 2 2] (freqs-of-ranks hand)) true
        :else false))

(defn straight? [hand]
  (let [five-cards (fn [n] (range n (+ n 5)))
        all-straights (map five-cards (range 2 11))
        sorted-ranks (sort (map rank hand))
        same-as-hand? (partial = sorted-ranks)]
    (or
;     (contains? (set (map same-as-hand? all-straights)) true)
     (some same-as-hand? all-straights)
     (= [2 3 4 5 14] sorted-ranks))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn value [hand]
  (cond
   (straight-flush? hand)  8
   (four-of-a-kind? hand)  7
   (full-house? hand)      6
   (flush? hand)           5
   (straight? hand)        4
   (three-of-a-kind? hand) 3
   (two-pairs? hand)       2
   (pair? hand)            1
   :else                   0))
