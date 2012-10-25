(ns p-p-p-pokerface)

(defn rank-value [rank]
  (let [ranks {\T 10, \J 11, \Q 12, \K 13, \A 14}]
    (get ranks rank)))

(defn rank [[value _]]
  (if (Character/isDigit value)
    (Integer/valueOf (str value))
    (rank-value value)))

(defn suit [[_ maa]]
  (str maa))

(defn ranks-in-hand [hand]
  (map rank hand))

(defn suits-in-hand [hand]
  (map suit hand))

(defn rank-frequencies [hand]
  (vals (frequencies (ranks-in-hand hand))))

(defn suit-frequencies [hand]
  (vals (frequencies (suits-in-hand hand))))

(defn max-of-seq [sekvenssi]
  (apply max sekvenssi))

(defn min-of-seq [sekvenssi]
  (apply min sekvenssi))

(defn pair? [hand]
  (>= (max-of-seq (rank-frequencies hand)) 2))

(defn three-of-a-kind? [hand]
  (>= (max-of-seq (rank-frequencies hand)) 3))

(defn four-of-a-kind? [hand]
  (>= (max-of-seq (rank-frequencies hand)) 4))

(defn flush? [hand]
  (== (max-of-seq (suit-frequencies hand)) 5))

(defn full-house? [hand]
  (= (sort (rank-frequencies hand)) (seq [2 3])))

(defn two-pairs? [hand]
  (or
   (= (sort (rank-frequencies hand)) (seq [1 2 2]))
   (= (sort (rank-frequencies hand)) (seq [1 4]))))

(defn straight? [hand]
  (let [arvot (ranks-in-hand hand)
        arvot2 (replace {14 1} arvot)] ; assa=1
    (or (= (- (max-of-seq arvot) (min-of-seq arvot)) 4)
        (= (- (max-of-seq arvot2) (min-of-seq arvot2)) 4))))

(defn straight-flush? [hand]
  (and (flush? hand) (straight? hand)))

(defn high-card? [hand]
  true)

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