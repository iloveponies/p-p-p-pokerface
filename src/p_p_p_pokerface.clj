(ns p-p-p-pokerface)

(defn rank [card]
  (let [[fst _] card
        high-rank {\T 10 \J 11 \Q 12 \K 13 \A 14}]
    (if (Character/isDigit fst)
      (Integer/valueOf (str fst))
      (high-rank fst))))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn has-cards? [hand n fn-card-type]
  (= n
     (some #{n}
           (vals (frequencies (map fn-card-type hand))))))



(defn pair? [hand]
  (has-cards? hand 2 rank))


(defn three-of-a-kind? [hand]
  (has-cards? hand 3 rank))

(defn four-of-a-kind? [hand]
  (has-cards? hand 4 rank))

(defn flush? [hand]
  (has-cards? hand 5 suit))

(defn full-house? [hand]
  (=
     (seq [2 3])
     (sort (vals (frequencies (map rank hand))))))

(defn two-pairs? [hand]
  (=
     (seq [1 2 2])
     (sort (vals (frequencies (map rank hand))))))


(defn straight? [hand]
  (let [sorted-ranks (sort (map rank hand))
        min-rank (apply min sorted-ranks)
        straight-ranks (range min-rank (+ 5 min-rank))
        sorted-ranks2 (sort (replace {14 1} (map rank hand)))
        straight-ranks2 (range 1 6)]
    (or
       (= straight-ranks sorted-ranks)
       (= straight-ranks2 sorted-ranks2)
       false)))



(defn straight-flush? [hand]
  (and
     (straight? hand)
     (flush? hand)
     true))

(defn value [hand]
  (cond
     (straight-flush? hand) 8
     (four-of-a-kind? hand) 7
     (full-house? hand ) 6
     (flush? hand) 5
     (straight? hand) 4
     (three-of-a-kind? hand) 3
     (two-pairs? hand) 2
     (pair? hand) 1
     :else 0))
