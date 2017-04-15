(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rank suit] card]
    (cond (Character/isDigit rank) (Integer/valueOf (str rank))
          :else (get {\T 10 \J 11 \Q 12 \K 13 \A 14} rank))))

(defn suit [card]
  (let [[rank suit] card]
    (str suit)))

(defn n-of-a-kind? [hand n]
  (>= (apply max (vals (frequencies (map rank hand)))) n))

(defn pair? [hand]
  (n-of-a-kind? hand 2))

(defn three-of-a-kind? [hand]
  (n-of-a-kind? hand 3))

(defn four-of-a-kind? [hand]
  (n-of-a-kind? hand 4))

(defn flush? [hand]
  (== (count (frequencies (map suit hand))) 1))

(defn full-house? [hand]
  (= (sort (vals (frequencies (map rank hand)))) [2 3]))

(defn two-pairs? [hand]
  (contains? (set [[1 2 2] [2 3] [1 4] [5]])
             (into [] (sort (vals (frequencies (map rank hand)))))))

(defn low-ace-rank [card]
  (let [rank (rank card)]
    (if (== rank 14) 1 rank)))

(defn number-straight? [s]
  (and (== (count (distinct s)) (count s))
       (== (- (apply max s) (apply min s)) (dec (count s)))))

(defn straight? [hand]
  (let [ranks (map rank hand)
        ranks2 (map low-ace-rank hand)]
    (or (number-straight? (map rank hand))
        (number-straight? (map low-ace-rank hand)))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn value [hand]
  (cond (straight-flush? hand) 8
        (four-of-a-kind? hand) 7
        (full-house? hand) 6
        (flush? hand) 5
        (straight? hand) 4
        (three-of-a-kind? hand) 3
        (two-pairs? hand) 2
        (pair? hand) 1
        :else 0))
