(ns p-p-p-pokerface)

(defn rank [card]
  (let [[fst _] card]
    (if (Character/isDigit fst)
      (Integer/valueOf (str fst))
      (get {\T 10 \J 11 \Q 12 \K 13 \A 14} fst))))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn pair? [hand]
  (let [rank-freq (vals (frequencies (map rank hand)))]
    (>= (apply max rank-freq) 2)))

(defn three-of-a-kind? [hand]
  (let [rank-freq (vals (frequencies (map rank hand)))]
    (>= (apply max rank-freq) 3)))

(defn four-of-a-kind? [hand]
  (let [rank-freq (vals (frequencies (map rank hand)))]
    (>= (apply max rank-freq) 4)))

(defn flush? [hand]
  (= 1 (count (set (map suit hand)))))

(defn full-house? [hand]
  (let [rank-freq (sort (vals (frequencies (map rank hand))))]
    (= [2 3] rank-freq)))

(defn two-pairs? [hand]
  (let [rank-freq (sort (vals (frequencies (map rank hand))))]
    (or (= [1 2 2] rank-freq)
        (four-of-a-kind? hand))))

(defn straight? [hand]
  (let [sorted-ranks-high-ace (sort (map rank hand))
        sorted-ranks-low-ace (sort (replace {14 1} (map rank hand)))]
    (or (= (range (apply min sorted-ranks-low-ace)
                  (+ 1 (apply max sorted-ranks-low-ace)))
           sorted-ranks-low-ace)
        (= (range (apply min sorted-ranks-high-ace)
                  (+ 1 (apply max sorted-ranks-high-ace)))
           sorted-ranks-high-ace))))

(defn straight-flush? [hand]
  (and (flush? hand)
       (straight? hand)))

(defn value [hand]
  (cond (straight-flush? hand) 8
        (four-of-a-kind? hand) 7
        (full-house? hand) 6
        (flush? hand) 5
        (straight? hand) 4
        (three-of-a-kind? hand) 3
        (two-pairs? hand) 2
        (pair? hand) 1
        :else 0))
