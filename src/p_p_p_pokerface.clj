(ns p-p-p-pokerface)

(defn rank [card]
  (def replacements {\T 10, \J 11 \Q 12 \K 13  \A 14})
  (let [[rank _] card]
    (if (Character/isDigit rank)
      (Integer/valueOf (str rank))
      (replacements rank))))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn pair? [hand]
  (let [hand-rank-freq (frequencies (map rank hand))
        hand-freq-vals (vals hand-rank-freq)]
    (>= (apply max hand-freq-vals) 2)))

(defn three-of-a-kind? [hand]
  (let [hand-rank-freq (frequencies (map rank hand))
        hand-freq-vals (vals hand-rank-freq)]
    (>= (apply max hand-freq-vals) 3)))

(defn four-of-a-kind? [hand]
  (let [hand-rank-freq (frequencies (map rank hand))
        hand-freq-vals (vals hand-rank-freq)]
    (>= (apply max hand-freq-vals) 4)))

(defn two-pairs? [hand]
  (let [hand-rank-freq (frequencies (map rank hand))
        hand-freq-vals (vals hand-rank-freq)
        bigger-than-1 (fn [x] (> x 1))]
    (or (= (count (filter bigger-than-1 hand-freq-vals)) 2)
        (four-of-a-kind? hand))))

(defn straight? [hand]
  (let [hand-ranks (sort (map rank hand))
        hand-ranks-replaced (sort (replace {14 1} hand-ranks))
        straight-range (range (first hand-ranks) 
                              (+ (first hand-ranks) 5))
        straight-range-replaced (range (first hand-ranks-replaced) 
                                       (+ (first hand-ranks-replaced) 5))]
    (or (= hand-ranks straight-range)
        (= hand-ranks-replaced straight-range-replaced))))

(defn flush? [hand]
  (let [hand-suit-freq (frequencies (map suit hand))
        hand-freq-vals (vals hand-suit-freq)]
    (= (apply max hand-freq-vals) 5)))

(defn full-house? [hand]
  (let [hand-rank-freq (frequencies (map rank hand))
        hand-freq-vals (vals hand-rank-freq)]
    (= [2 3] (sort hand-freq-vals))))

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
