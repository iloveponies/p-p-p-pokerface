(ns p-p-p-pokerface)

(def card-values { \T 10, \J 11, \Q 12, \K 13, \A 14 })

(defn rank [card]
  (let [[fst _] card]
    (if (Character/isDigit fst) (Integer/valueOf (str fst)) (card-values fst))))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn pair? [hand]
  (let [hand-ranks (map rank hand)
        max-pair (apply max (vals (frequencies hand-ranks)))]
    (== max-pair 2)))

(defn three-of-a-kind? [hand]
  (let [hand-ranks (map rank hand)
        max-three (apply max (vals (frequencies hand-ranks)))]
    (== max-three 3)))

(defn four-of-a-kind? [hand]
  (let [hand-ranks (map rank hand)
        max-four (apply max (vals (frequencies hand-ranks)))]
    (== max-four 4)))

(defn flush? [hand]
  (let [hand-suits (map suit hand)
        max-suit (apply max (vals (frequencies hand-suits)))]
    (== max-suit 5)))

(defn full-house? [hand]
  (let [sorted-hand-values (sort (vals (frequencies (map rank hand))))]
    (= sorted-hand-values [2,3])))

(defn two-pairs? [hand]
  (let [sorted-hand-values (sort (vals (frequencies (map rank hand))))]
    (or (= sorted-hand-values [1, 2, 2]) (= sorted-hand-values [1, 4]))))

(defn straight? [hand]
  (let [sorted-hand-ranks (sort(map rank hand))
        min-val (apply min sorted-hand-ranks)
        max-val (apply max sorted-hand-ranks)
        rank-seq (range min-val (+ max-val 1))]
    (or (= sorted-hand-ranks rank-seq) (= sorted-hand-ranks [2,3,4,5,14]))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn value [hand]
  (cond
    (and (pair? hand) (not (two-pairs? hand))) 1
    (and (two-pairs? hand) (not (four-of-a-kind? hand))) 2
    (and (three-of-a-kind? hand) (not (full-house? hand))) 3
    (and (straight? hand) (not (flush? hand))) 4
    (and (flush? hand) (not (straight? hand))) 5
    (full-house? hand) 6
    (four-of-a-kind? hand) 7
    (straight-flush? hand) 8
    :else 0))
