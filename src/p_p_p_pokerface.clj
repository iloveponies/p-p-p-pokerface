(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rank _]   card
        rank-value {\T 10 \J 11 \Q 12 \K 13 \A 14}]
    (if (Character/isDigit rank)
      (Integer/valueOf (str rank))
      (rank-value rank))))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn rank-frequencies [hand]
  (frequencies (map rank hand)))

(defn suit-frequencies [hand]
  (frequencies (map suit hand)))

(defn n-of-a-kind? [n hand]
  (let [rank-freq (vals (rank-frequencies hand))]
    (== (apply max rank-freq) n)))

(defn pair? [hand]
  (n-of-a-kind? 2 hand))

(defn three-of-a-kind? [hand]
  (n-of-a-kind? 3 hand))

(defn four-of-a-kind? [hand]
  (n-of-a-kind? 4 hand))

(defn flush? [hand]
  (let [suit-freq (vals (suit-frequencies hand))]
    (== (apply max suit-freq) 5)))

(defn full-house? [hand]
  (= (sort (vals (rank-frequencies hand)))
     (seq [2 3])))

(defn two-pairs? [hand]
  (= (sort (vals (rank-frequencies hand)))
     (seq [1 2 2])))

(defn straight? [hand]
  (let [ranks   (sort (keys (rank-frequencies hand)))
        ranks1  (sort (replace {14 1} ranks))
        ranks14 (sort (replace {1 14} ranks))
        low     (first ranks)]
    (or
      (= ranks   (range low (+ low 5)))
      (= ranks1  (range 1 6))
      (= ranks14 (range 10 15)))))

(defn straight-flush? [hand]
  (and (straight? hand)
       (flush? hand)))

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
