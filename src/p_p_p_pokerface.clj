(ns p-p-p-pokerface)

(defn rank [card]
  (let [ch-ranks {\T 10, \J 11, \Q 12, \K 13, \A 14}
        [fst _] card]
        (if (Character/isDigit fst)
          (Integer/valueOf (str fst))
          (get ch-ranks fst))))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn rank-freq [hand]
  (vals (frequencies (map rank hand))))

(defn pair? [hand]
  (>= (apply max (rank-freq hand)) 2))

(defn three-of-a-kind? [hand]
  (>= (apply max (rank-freq hand)) 3))

(defn four-of-a-kind? [hand]
  (>=  (apply max (rank-freq hand)) 4))

(defn flush? [hand]
  (== 5 (apply max (vals (frequencies (map suit hand))))))

(defn full-house? [hand]
  (= (sort (rank-freq hand)) [2 3]))

(defn two-pairs? [hand]
 (or (four-of-a-kind? hand) (= (sort (rank-freq hand)) [1 2 2])))

(defn straight? [hand]
  (let [sorted (sort (map rank hand))
        sorted-ace (replace {14 1} sorted)
        min-s (apply min sorted)
        max-s (apply max sorted)
        min-sa (apply min sorted-ace)
        max-sa (apply max sorted-ace)]
    (and (not (pair? hand))
         (or (= 5 (count (range min-s (+ 1 max-s))))
             (= 5 (count (range min-sa (+ 1 max-sa))))))))

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
