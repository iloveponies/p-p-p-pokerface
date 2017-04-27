(ns p-p-p-pokerface)

(defn rank-to-integer [rank]
  (let [mapping {\T 10, \J 11, \Q 12, \K, 13 \A 14}]
    (Integer/valueOf (str (get mapping rank)))))

(defn rank [card]
  (let [[rank suite] card]
    (if (Character/isDigit rank)
      (Integer/valueOf (str rank))
      (rank-to-integer rank))))

(defn suit [card]
  (let [[rank suit] card]
    (str suit)))

(defn pair? [hand]
  (if (> (apply max (vals (frequencies (map rank hand)))) 1)
    true
    false))

(defn three-of-a-kind? [hand]
  (if (> (apply max (vals (frequencies (map rank hand)))) 2)
    true
    false))

(defn four-of-a-kind? [hand]
  (if (> (apply max (vals (frequencies (map rank hand)))) 3)
    true
    false))

(defn flush? [hand]
  (if (> (apply max (vals (frequencies (map suit hand)))) 4)
    true
    false))

(defn full-house? [hand]
  (= [2 3] (sort (vals (frequencies (map rank hand))))))

(defn two-pairs? [hand]
  (or
    (= [1 2 2] (sort (vals (frequencies (map rank hand)))))
    (= [1 4] (sort (vals (frequencies (map rank hand))))))
  )

(defn straight? [hand]
  (def low-ace [2 3 4 5 14])
  (let [sorted-ranks (sort (map rank hand))
        min-rank (apply min sorted-ranks)
        test-ranks (range min-rank (+ min-rank 5))]
    (or (= sorted-ranks low-ace) (= sorted-ranks test-ranks))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

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
