(ns p-p-p-pokerface)

(def replacements {\T 10, \J 11, \Q 12, \K 13, \A 14})

(defn rank [card]
  (let [[fst _] card]
    (if (Character/isDigit fst)
      (Integer/valueOf (str fst))
      (replacements fst))))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn rank-frequencies [hand]
  (vals (frequencies (map rank hand))))

(defn suit-frequencies [hand]
  (vals (frequencies (map suit hand))))

(defn pair? [hand]
  (< 1 (apply max (rank-frequencies hand))))

(defn three-of-a-kind? [hand]
  (< 2 (apply max (rank-frequencies hand))))

(defn four-of-a-kind? [hand]
  (< 3 (apply max (rank-frequencies hand))))

(defn flush? [hand]
  (< 4 (apply max (suit-frequencies hand))))

(defn full-house? [hand]
  (= [2 3] (sort (rank-frequencies hand))))

(defn two-pairs? [hand]
  (or (four-of-a-kind? hand) (= [1 2 2] (sort (rank-frequencies hand)))))

(defn straight? [hand]
  (let [[a b c d e] (sort (map rank hand))]
    (or (= [2 3 4 5 14] [a b c d e])
        (and (= [1 1 1 1 1] (sort (rank-frequencies hand))) (= 4 (- e a))))))

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
