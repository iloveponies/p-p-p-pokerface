(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rank-char _] card
        replacements {\T 10 \J 11 \Q 12 \K 13 \A 14}]
    (cond
      (Character/isDigit rank-char) (Integer/valueOf (str rank-char))
      :else (get replacements rank-char))))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn rank-repeat-count [hand]
  (vals (frequencies (map rank hand))))

(defn suit-repeat-count [hand]
  (vals (frequencies (map suit hand))))

(defn has-n-repeats? [hand n]
  (< 0 (count (filter (fn [count] (= n count)) (rank-repeat-count hand)))))

(defn pair? [hand]
  (has-n-repeats? hand 2))

(defn three-of-a-kind? [hand]
  (has-n-repeats? hand 3))

(defn four-of-a-kind? [hand]
  (has-n-repeats? hand 4))

(defn flush? [hand]
  (= 5 (apply max (suit-repeat-count hand))))

(defn full-house? [hand]
  (and (pair? hand) (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  (or (four-of-a-kind? hand)
      (= '(2 2) (filter (fn [repeat] (= 2 repeat)) (rank-repeat-count hand)))))

(defn sequential-ranks [rank-seq]
  (range (apply min rank-seq) (+ 1( apply max rank-seq))))

(defn straight? [hand]
  (let [r (sort (map rank hand))
        r-generated (sequential-ranks r)
        r-ace-low (sort (replace {14 1} r))
        r-ace-low-generated (sequential-ranks r-ace-low)]
    (or (= r r-generated)
        (= r-ace-low r-ace-low-generated))))

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
