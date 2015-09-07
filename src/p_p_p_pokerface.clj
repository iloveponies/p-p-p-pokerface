(ns p-p-p-pokerface)

(defn rank [card]
  (let [[card-value _] card]
    (def high-cards {\T 10, \J 11, \Q 12, \K 13, \A 14})

    (if (Character/isDigit card-value)
      (Integer/valueOf (str card-value))
      (Integer/valueOf (str (high-cards card-value))))))

(defn suit [card]
  (let [[_ card-suit] card]
    (str card-suit)))

(defn rank-frequency [hand minOrMax]
  (apply minOrMax (vals (frequencies (map rank hand)))))

(defn pair? [hand]
  (== (rank-frequency hand max) 2))

(defn three-of-a-kind? [hand]
  (== (rank-frequency hand max) 3))

(defn four-of-a-kind? [hand]
  (== (rank-frequency hand max) 4))

(defn flush? [hand]
  (== (apply max (vals (frequencies (map suit hand)))) 5))

(defn full-house? [hand]
  (and (== (rank-frequency hand max) 3) (== (rank-frequency hand min) 2)))

(defn two-pairs? [hand]
  (=
   (sort(vals (frequencies (map rank hand))))
   (seq [1 2 2])))

(defn straight? [hand]
  (or
     ; top ace
     (= (sort(map rank hand))
       (range
         (apply min (sort(map rank hand)))
         (+(apply max (sort(map rank hand))) 1)))

     ; low ace
     (= (sort (replace {14 1} (map rank hand)))
       (range
         (apply min (sort (replace {14 1} (map rank hand))))
         (+(apply max (sort (replace {14 1} (map rank hand)))) 1)))))


(defn straight-flush? [hand]
  (and
    (straight? hand)
    (flush? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (cond
   (straight-flush? hand)   8
   (four-of-a-kind? hand)   7
   (full-house? hand)       6
   (flush? hand)            5
   (straight? hand)         4
   (three-of-a-kind? hand)  3
   (two-pairs? hand)        2
   (pair? hand)             1
   (high-card? hand)        0))
