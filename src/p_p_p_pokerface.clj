(ns p-p-p-pokerface)

(defn rank [card]
  (def matches {\T 10, \J 11, \Q 12, \K 13, \A 14})
  (let [[r _] card]
    (if (Character/isDigit r)
      (Integer/valueOf (str r))
      (matches r))))

(defn suit [card]
  (let [[_ s] card]
    (str s)))

(defn pair? [hand]
  (= (apply max (vals (frequencies (map rank hand)))) 2))

(defn three-of-a-kind? [hand]
  (= (apply max (vals (frequencies (map rank hand)))) 3))

(defn four-of-a-kind? [hand]
  (= (apply max (vals (frequencies (map rank hand)))) 4))

(defn flush? [hand]
  (= (count (vals (frequencies (map suit hand)))) 1))

(defn full-house? [hand]
  (and (= (apply max (vals (frequencies (map rank hand)))) 3)
       (= (count (vals (frequencies (map rank hand)))) 2)))

(defn two-pairs? [hand]
  (and (= (apply max (vals (frequencies (map rank hand)))) 2)
       (= (count (vals (frequencies (map rank hand)))) 3)))

(defn straight? [hand]
  (def low-ace [2 3 4 5 14])
  (let [sorted-ranks (sort (map rank hand))
        min-rank (apply min sorted-ranks)
        test-ranks (range min-rank (+ min-rank 5))]
    (or (= sorted-ranks low-ace) (= sorted-ranks test-ranks))))

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
