(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rank _] card
        face-cards {\T 10, \J 11, \Q 12, \K 13, \A 14}]
    (if (Character/isDigit rank)
      (Integer/valueOf (str rank))
      (get face-cards rank))))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn pair? [hand]
  (> (apply max (vals (frequencies (map rank hand)))) 1))

(defn three-of-a-kind? [hand]
  (> (apply max (vals (frequencies (map rank hand)))) 2))

(defn four-of-a-kind? [hand]
  (> (apply max (vals (frequencies (map rank hand)))) 3))

(defn flush? [hand]
  (== (apply max (vals (frequencies (map suit hand)))) 5))

(defn full-house? [hand]
  (= [2 3] (sort (vals (frequencies (map rank hand))))))

(defn two-pairs? [hand]
  (let [rank-counts (sort (vals (frequencies (map rank hand))))]
    (or (= [1 2 2] rank-counts) (= [1 4] rank-counts))))

(defn straight? [hand]
  (let [sorted-ranks-hi-ace (sort (map rank hand))
        sorted-ranks-lo-ace (sort (replace {14 1} sorted-ranks-hi-ace))
        min-rank-hi-ace (apply min sorted-ranks-hi-ace)
        min-rank-lo-ace (apply min sorted-ranks-lo-ace)]
    (or
     (= sorted-ranks-hi-ace (range min-rank-hi-ace (+ min-rank-hi-ace 5)))
     (= sorted-ranks-lo-ace (range min-rank-lo-ace (+ min-rank-lo-ace 5))))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

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
