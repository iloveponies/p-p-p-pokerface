(ns p-p-p-pokerface)

(defn rank [card]
  (let [c (first card)]
    (if (Character/isDigit c)
      (Integer/valueOf (str c))
      (get {\T 10, \J 11, \Q 12, \K 13, \A 14} c))))

(defn suit [card]
  (str (second card)))

(defn pair? [hand]
  (->> (map rank hand)
       frequencies
       vals
       (some #(= % 2))
       boolean))

(defn three-of-a-kind? [hand]
  (->> (map rank hand)
       frequencies
       vals
       (some #(= % 3))
       boolean))

(defn four-of-a-kind? [hand]
  (->> (map rank hand)
       frequencies
       vals
       (some #(= % 4))
       boolean))

(defn flush? [hand]
  (= 1
     (->> (map suit hand)
          set
          count)))


(defn full-house? [hand]
  (and (pair? hand) (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  (->> (map rank hand)
       frequencies
       vals
       (filter #(= % 2))
       count
       (#(= % 2))))

(defn straight? [hand]
  (let [sorted-ranks (sort (map rank hand))
        min-rank     (first sorted-ranks)]
    (or (= sorted-ranks
           (range min-rank (+ min-rank 5)))
        (= sorted-ranks
           [2 3 4 5 14]))))

(defn straight-flush? [hand]
  (and (straight? hand)
       (flush? hand)))

(defn value [hand]
  (cond (straight-flush?  hand) 8
        (four-of-a-kind?  hand) 7
        (full-house?      hand) 6
        (flush?           hand) 5
        (straight?        hand) 4
        (three-of-a-kind? hand) 3
        (two-pairs?       hand) 2
        (pair?            hand) 1
        :else                   0))
