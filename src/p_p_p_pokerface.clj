(ns p-p-p-pokerface)

(defn rank [card]
  (let [[-rank _] card
        special-ranks {\T 10, \J 11, \Q 12, \K 13, \A 14}]
    (if (Character/isDigit -rank)
      (Integer/valueOf (str -rank))
      (special-ranks -rank))))


(defn suit [card]
  (let [[_ -suit] card]
    (str -suit)))

(defn repetitions [pred hand]
  (vals (frequencies (map pred hand))))

(defn same-rank [hand]
  (apply max (repetitions rank hand)))

(defn same-suit [hand]
  (apply max (repetitions suit hand)))

(defn pair? [hand]
  (= 2 (same-rank hand)))

(defn three-of-a-kind? [hand]
  (= 3 (same-rank hand)))

(defn four-of-a-kind? [hand]
  (= 4 (same-rank hand)))

(defn flush? [hand]
  (= 5 (same-suit hand)))

(defn full-house? [hand]
  (let [full-house-hand '(2 3)]
    (= full-house-hand (sort (repetitions rank hand)))))

(defn two-pairs? [hand]
  (let [two-pairs-hand '(1 2 2)]
    (= two-pairs-hand (sort (repetitions rank hand)))))

(defn straight? [hand]
  (let [ranks (map rank hand)
        normalized-ranks (if (contains? (set ranks) 2)
                           (replace {14 1} ranks)
                           ranks)
        lower-rank (apply min normalized-ranks)]
    (= (sort normalized-ranks) (range lower-rank (+ lower-rank 5)))))


(defn straight-flush? [hand]
  (and (flush? hand) (straight? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                   [two-pairs? 2]  [three-of-a-kind? 3]
                   [straight? 4]   [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}]
    (apply max
           (map (fn [[checker value]]
                  (if (checker hand) value 0))
                checkers))))
