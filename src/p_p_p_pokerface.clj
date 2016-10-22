(ns p-p-p-pokerface)

(def replacements {\T 10, \J 11, \Q 12, \K 13, \A 14})

(defn rank [card]
  (let [[fst _] card]
    (if (Character/isDigit fst)
      (Integer/valueOf (str fst))
      (Integer/valueOf (str (replacements fst))))))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn ranks [hand]
  (mapv rank hand))

(defn suits [hand]
  (mapv suit hand))

(defn samerank [hand]
  (apply max(vals (frequencies (ranks hand)))))

(defn samesuit [hand]
  (apply max(vals (frequencies (suits hand)))))

(defn pair? [hand]
  (>= (samerank hand) 2))

(defn three-of-a-kind? [hand]
  (>= (samerank hand) 3))

(defn four-of-a-kind? [hand]
  (>= (samerank hand) 4))

(defn flush? [hand]
  (= (samesuit hand) 5))

(defn full-house? [hand]
    (and (= 2 (apply min (vals (frequencies (ranks hand)))))
         (= 3 (samerank hand))))

(defn two-pairs? [hand]
  (or (four-of-a-kind? hand)
      (full-house? hand)
      (= [1 2 2] (sort (vals (frequencies (ranks hand)))))))

(defn straight? [hand]
  (let [rank-min (fn [some-rank] (apply min some-rank))
        ace-rank-sort (fn [hand] (sort (ranks hand)))
        ace-repl-rank-sort (fn [hand] (sort (replace {14 1} (ranks hand))))]
    (or (= (ace-rank-sort hand) (range (rank-min (ace-rank-sort hand)) (+ (rank-min (ace-rank-sort hand)) 5)))
        (= (ace-repl-rank-sort hand) (range (rank-min (ace-repl-rank-sort hand)) (+ (rank-min (ace-repl-rank-sort hand)) 5))))))

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
