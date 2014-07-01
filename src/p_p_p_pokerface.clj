(ns p-p-p-pokerface)

(defn rank [card]
  (let [value (first card)]
    (if (Character/isDigit value)
      (Integer/valueOf (str value))
      (get {\T 10, \J 11, \Q 12, \K 13, \A 14} value))))

(defn suit [card]
  (str (second card)))

(defn rank-freq [hand]
  (vec (vals (frequencies (map rank hand)))))

(defn n-of-a-kind? [hand n]
  (= 1 (count (filter (fn [r] (= n r)) (rank-freq hand)))))

(defn pair? [hand]
  (n-of-a-kind? hand 2))

(defn three-of-a-kind? [hand]
  (n-of-a-kind? hand 3))

(defn four-of-a-kind? [hand]
  (n-of-a-kind? hand 4))

(defn flush? [hand]
  (= 1 (count (set (map suit hand)))))

(defn full-house? [hand]
  (and (n-of-a-kind? hand 3) (n-of-a-kind? hand 2)))

(defn two-pairs? [hand]
  (= 2 (get (frequencies (rank-freq hand)) 2)))

(defn straight? [hand]
  (let [seq-1 (sort (map rank hand))
        seq-2 (sort (replace {14 1} seq-1))
        seq-1-first-card (first seq-1)
        seq-2-first-card (first seq-2)]
    (or
      (= seq-1 (range seq-1-first-card (+ seq-1-first-card 5)))
      (= seq-2 (range seq-2-first-card (+ seq-2-first-card 5))))))

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
