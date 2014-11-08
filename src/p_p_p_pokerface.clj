(ns p-p-p-pokerface)

(defn rank [card]
  (let [replacements {\T 10, \J 11, \Q 12, \K 13, \A 14}
        [rank-char _] card]
    (if (Character/isDigit rank-char)
      (Integer/valueOf (str rank-char))
      (replacements rank-char))))

(defn suit [card]
  (let [[_ card-suit] card]
    (str card-suit)))

(defn pair? [hand]
  (>= (apply max (vals (frequencies (map rank hand)))) 2))

(defn three-of-a-kind? [hand]
  (>= (apply max (vals (frequencies (map rank hand)))) 3))

(defn four-of-a-kind? [hand]
  (>= (apply max (vals (frequencies (map rank hand)))) 4))

(defn flush? [hand]
  (apply = (map suit hand)))

(defn full-house? [hand]
  (= (sort (vals (frequencies (map rank hand)))) (seq [2 3])))

(defn two-pairs? [hand]
  (let [sorted-frequencies (sort (vals (frequencies (map rank hand))))]
    (or (= sorted-frequencies (seq [1 2 2]))
        (= sorted-frequencies (seq [1 4])))))

(defn straight? [hand]
  (let [sorted-ranks (sort (map rank hand))
        lowest-rank (first sorted-ranks)]
    (or (= sorted-ranks (range lowest-rank (+ lowest-rank 5)))
        (= sorted-ranks (seq [2 3 4 5 14])))))

(defn straight-flush? [hand]
  (and (flush? hand) (straight? hand)))

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
