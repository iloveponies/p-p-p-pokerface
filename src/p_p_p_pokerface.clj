(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rank suit] card
        numbers-for-letters {\T 10, \J 11, \Q 12, \K 13, \A 14}]
    (if (Character/isDigit rank)
      (Integer/valueOf (str rank))
      (numbers-for-letters rank))))

(defn suit [card]
  (let [[rank suit] card]
    (str suit)))

(defn contains-multiplicity? [hand, multiplicity]
  (not (= nil (some #{multiplicity} (vals (frequencies (map rank hand)))))))

(defn pair? [hand]
  (contains-multiplicity? hand 2))

(defn three-of-a-kind? [hand]
  (contains-multiplicity? hand 3))

(defn four-of-a-kind? [hand]
  (contains-multiplicity? hand 4))

(defn flush? [hand]
  (== 5 (apply min (vals (frequencies (map suit hand))))))

(defn full-house? [hand]
  (and (contains-multiplicity? hand 3) (contains-multiplicity? hand 2)))

(defn two-pairs? [hand]
  (let [pair? (fn [multiplicity] (== 2 multiplicity))]
    (or (== 2 (count (filter pair? (vals (frequencies (map rank hand))))))
        (four-of-a-kind? hand))))

(defn straight? [hand]
  (let [equal-to-range? (fn [sequence] (= sequence
                                       ;;sorted is a sequence, so we have to apply min with all its elements
                                       (range (apply min sequence) (+ 5 (apply min sequence)))))]
    (or (equal-to-range? (sort (replace {14 1} (map rank hand))))
        (equal-to-range? (sort (map rank hand))))))

(defn straight-flush? [hand]
  (and (flush? hand) (straight? hand)))

(defn value [hand]
  (cond (straight-flush? hand) 8 (four-of-a-kind? hand) 7 (full-house? hand) 6 (flush? hand) 5
        (straight? hand) 4 (three-of-a-kind? hand) 3 (two-pairs? hand) 2 (pair? hand) 1 :else 0))
