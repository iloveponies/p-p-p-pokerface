(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rank] card]
    (if (Character/isDigit rank)
      (Integer/valueOf (str rank))
      ({\T 10 \J 11 \Q 12 \K 13 \A 14} rank))))

(defn suit [card]
  (let [[rank suit] card]
    (str suit)))

(defn pair? [hand]
  (>= (apply max (vals (frequencies (map rank hand)))) 2))

(defn three-of-a-kind? [hand]
  (>= (apply max (vals (frequencies (map rank hand)))) 3))

(defn four-of-a-kind? [hand]
  (>= (apply max (vals (frequencies (map rank hand)))) 4))

(defn flush? [hand]
  (apply = (map suit hand)))

(defn full-house? [hand]
  (= (sort (vals (frequencies (map rank hand)))) [2 3]))

(defn two-pairs? [hand]
  (or
    (four-of-a-kind? hand)
    (= (sort (vals (frequencies (map rank hand)))) [1 2 2])))

(defn straight? [hand]
  (let [[sorted] [(sort (map rank hand))]
        [sorted-min] [(first sorted)]
        [sorted-rep] [(sort (replace {14 1} (map rank hand)))]
        [sorted-rep-min] [(first sorted-rep)]]
    (and
      (or
        (= sorted (range sorted-min (+ sorted-min 5)))
        (= sorted-rep (range sorted-rep-min (+ sorted-rep-min 5)))))))

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