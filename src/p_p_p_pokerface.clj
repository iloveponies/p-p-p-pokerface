(ns p-p-p-pokerface)

(defn rank [[rank _]]
  (let [rank-num {\T 10, \J 11, \Q 12, \K 13, \A 14}]
      (if (contains? rank-num rank)
        (get rank-num rank) 
        (Integer/valueOf (str rank)))))

(defn suit [[_ suit]]
  (str suit))

(defn pair? [hand]
  (>= (apply max (vals (frequencies (map rank hand)))) 2))

(defn three-of-a-kind? [hand]
  (>= (apply max (vals (frequencies (map rank hand)))) 3))

(defn four-of-a-kind? [hand]
  (>= (apply max (vals (frequencies (map rank hand)))) 4))

(defn flush? [hand]
  (= 1 (count (frequencies (map suit hand)))))

(defn full-house? [hand]
  (= [2 3] (sort (vals (frequencies (map rank hand))))))

(defn two-pairs? [hand]
  (or 
    (= [1 2 2] (sort (vals (frequencies (map rank hand)))))
    (four-of-a-kind? hand)))

(defn straight? [hand]
  (let [ranks (sort (map rank hand))
        ranks2 (sort (replace {14 1} ranks))
        min-ranks (apply min ranks)
        min-ranks2 (apply min ranks2)]
    (or
      (= (range min-ranks (+ min-ranks 5)) ranks)
      (= (range min-ranks2 (+ min-ranks2 5)) ranks2))))

(defn straight-flush? [hand]
 (and
    (flush? hand)
    (straight? hand)))

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
