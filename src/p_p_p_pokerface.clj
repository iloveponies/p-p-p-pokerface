(ns p-p-p-pokerface)

(defn rank [card]
  (let [high-ranks {\T 10, \J 11, \Q 12, \K 13, \A 14}
        [r _] card]
    (Integer/valueOf
      (str (if (Character/isDigit r) r (high-ranks r))))))

(defn suit [card]
  (let [[_ s] card]
   (str s)))

(defn pair? [hand]
  (== 2  (apply max
                (vals (frequencies (map rank hand))))))

(defn three-of-a-kind? [hand]
  (== 3  (apply max
                (vals (frequencies (map rank hand)))))) 

(defn four-of-a-kind? [hand]
  (== 4  (apply max
                (vals (frequencies (map rank hand)))))) 

(defn flush? [hand]
  (== 5 (apply max
               (vals (frequencies (map suit hand))))))

(defn full-house? [hand]
  (= [2 3] (sort (vals (frequencies (map rank hand))))))

(defn two-pairs? [hand]
  (let [hand-ranks (sort (vals (frequencies (map rank hand))))]
    (or (= [1 2 2] hand-ranks)(== 4 (apply max hand-ranks)))))
    
(defn straight? [hand]
  (let [ranks (sort (map rank hand))
        ranks2 (sort (replace {14 1} ranks))
        min-rank (first ranks)]
    (or (= (range min-rank (+ min-rank 5)) ranks)
        (= (range 1 6) ranks2))))

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
