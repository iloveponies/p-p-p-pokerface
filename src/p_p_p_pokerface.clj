(ns p-p-p-pokerface)

(def map-of-ranks {\T 10, \J 11, \Q 12, \K 13, \A 14})

(defn rank [card]
  (let [[rank _] card]
    (if (Character/isDigit rank)
      (Integer/valueOf (str rank))
      (map-of-ranks rank))))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

;---------- Helper functions! ----------
(defn ranks-of-hand [hand]
  (map rank hand))

(defn nrs-of-same-rank [hand]
  (vals (frequencies (ranks-of-hand hand))))

(defn nrs-of-same-suit [hand]
  (let [suits-of-hand (map suit hand)]
    (vals (frequencies suits-of-hand))))

(defn remapped-literals [hand]
  (replace map-of-ranks (ranks-of-hand hand)))

(defn ace-as-1 [hand]
  (replace {14 1} hand))

;------ End of helper functions! -------
(defn high-card? [_]
  true)
(defn pair? [hand]
  (contains? (set (nrs-of-same-rank hand)) 2))

(defn three-of-a-kind? [hand]
  (contains? (set (nrs-of-same-rank hand)) 3))

(defn four-of-a-kind? [hand]
  (contains? (set (nrs-of-same-rank hand)) 4))

(defn flush? [hand]
  (contains? (set (nrs-of-same-suit hand)) 5))

(defn full-house? [hand]
  (and (three-of-a-kind? hand) (pair? hand)))

(defn two-pairs? [hand]
  (== (count (filter (fn [x] (== x 2)) (nrs-of-same-rank hand))) 2))

(defn straight? [hand]
  (let [contains-both-ace-and-2? (and (contains? (set (remapped-literals hand)) 2)
                                      (contains? (set (remapped-literals hand)) 14))
        sorted-hand (sort (remapped-literals hand))
        sorted-hand-with-low-ace (sort (ace-as-1 sorted-hand))
        min-value (apply min (remapped-literals hand))
        max-value (apply max (remapped-literals hand))]
    (if contains-both-ace-and-2?
      (= (range 1 6) sorted-hand-with-low-ace)
      (= (range min-value (+ max-value 1)) sorted-hand))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(def checkers #{[high-card? 0] [pair? 1]
                [two-pairs? 2] [three-of-a-kind? 3]
                [straight? 4] [flush? 5]
                [full-house? 6] [four-of-a-kind? 7]
                [straight-flush? 8]})

(defn value [hand]
  (let [passed-checks (filter (fn [[checker _]] (checker hand)) checkers)
        values-of-passed-checks (map second passed-checks)]
    (apply max values-of-passed-checks)))

