(ns p-p-p-pokerface)

(def ranks {\T 10, \J 11, \Q 12, \K 13 \A 14})

(defn rank [card]
  (let [[rank _] card]
    (if (Character/isDigit rank)
      (Integer/valueOf (str rank))
      (ranks rank))))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn kinds-more-then? [hand n]
  (let [hand-ranks (map rank hand)
        maxx (apply max (vals (frequencies hand-ranks)))]
    (>= maxx n)))

(defn pair? [hand]
    (kinds-more-then? hand 2))

(defn three-of-a-kind? [hand]
    (kinds-more-then? hand 3))

(defn four-of-a-kind? [hand]
  (kinds-more-then? hand 4))

(defn flush? [hand]
  (let [hand-suits (map suit hand)
        maxx (apply max (vals (frequencies hand-suits)))]
    (= maxx 5)))

(defn full-house? [hand]
  (let [hand-ranks (map rank hand)
        frequencies-set (vals (frequencies hand-ranks))]
    (= [2 3] (sort frequencies-set))))

(defn two-pairs? [hand]
  (let [hand-ranks (map rank hand)
        frequencies-set (vals (frequencies hand-ranks))]
    (= [1 2 2] (sort frequencies-set))))

(defn straight? [hand]
  (let [hand-ranks (sort (map rank hand))
        hand-reverse (sort (replace {14 1} hand-ranks))
        range-fst (range (first hand-ranks) (+ 5 (first hand-ranks)))
        range-snd (range (first hand-reverse) (+ 5 (first hand-reverse)))]
    (or (= hand-ranks range-fst) (= hand-reverse range-snd))))

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
