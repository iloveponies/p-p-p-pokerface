(ns p-p-p-pokerface)

(defn rank [card]
  (let [[fst _] card]
    (if (Character/isDigit fst)
      (Integer/valueOf (str fst))
      (get {\T 10 \J 11 \Q 12 \K 13 \A 14} fst))))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn rank-counts [hand]
  (vals (frequencies (map rank hand))))

(defn pair? [hand]
  (contains? (set (rank-counts hand)) 2))

(defn three-of-a-kind? [hand]
  (contains? (set (rank-counts hand)) 3))

(defn four-of-a-kind? [hand]
  (contains? (set (rank-counts hand)) 4))

(defn suit-counts [hand]
  (vals (frequencies (map suit hand))))

(defn flush? [hand]
  (contains? (set (suit-counts hand)) 5))

(defn full-house? [hand]
  (and (contains? (set (rank-counts hand)) 2)
       (contains? (set (rank-counts hand)) 3)))

(defn two-pairs? [hand]
  (def hand hand)
  (or (boolean (= (count (filter #(= % 2) (rank-counts hand))) 2))
      (contains? (set (rank-counts hand)) 4)))

(defn straight? [hand]
  (def hand hand)
  (def sorted-hand (sort (map rank hand)))
  (let [sorted-hand (sort (map rank hand))
        ranks-in-hand (if (= sorted-hand [2 3 4 5 14])
                        [1 2 3 4 5]
                        sorted-hand)
        lowest-rank (first ranks-in-hand)]
    (= ranks-in-hand (range lowest-rank (+ lowest-rank 5)))))
(defn straight-flush? [hand]
  (and (straight? hand)
       (flush? hand)))

(defn high-card? [hand]
  true) ; All hands have a high card.

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
    (high-card? hand) 0))
