(ns p-p-p-pokerface)

(defn rank [card]
  (let [[fst _] card
        rank-values {\T 10, \J 11, \Q 12, \K 13, \A 14}]
    (if (Character/isDigit fst)
      (Integer/valueOf (str fst))
      (get rank-values fst))))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn x-of-a-kind-count [hand x]
  (let [ranks (map rank hand)]
    (count (filter #(= x %) (vals (frequencies ranks))))))

(defn pair? [hand]
  (= 1 (x-of-a-kind-count hand 2)))

(defn three-of-a-kind? [hand]
  (= 1 (x-of-a-kind-count hand 3)))

(defn four-of-a-kind? [hand]
  (= 1 (x-of-a-kind-count hand 4)))

(defn flush? [hand]
  (= 1 (count (frequencies (map suit hand)))))

(defn full-house? [hand]
  (and (pair? hand) (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  (or (four-of-a-kind? hand)
      (= 2 (x-of-a-kind-count hand 2))))

(defn straight? [hand]
  (let [ranks (map rank hand)
        max-rank (apply max ranks)
        min-rank (apply min ranks)
        alt-ranks (replace {14 1} ranks) ; when ace can be 1
        alt-max-rank (apply max alt-ranks)
        alt-min-rank (apply min alt-ranks)]
    (or (= (range min-rank (+ 1 max-rank))
           (sort ranks))
        (= (range alt-min-rank (+ 1 alt-max-rank))
           (sort alt-ranks)))))

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

(defn hand [a-hand]
  a-hand)

(defn card [a-card]
  a-card)
