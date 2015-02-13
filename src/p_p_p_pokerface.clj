(ns p-p-p-pokerface)

(defn rank [card]
  (let [[fst snd] card]
    (if (Character/isDigit fst)
      (Integer/valueOf (str fst))
      ({\A 14, \T 10, \J, 11, \Q, 12, \K 13} fst)
      ))
  )

(defn suit [card]
  (let [[fst snd] card]
    (str snd))
  )

(defn pair? [hand]
  (or
   (= (rank (hand 0)) (rank (hand 1)))
   (= (rank (hand 0)) (rank (hand 2)))
   (= (rank (hand 0)) (rank (hand 3)))
   (= (rank (hand 0)) (rank (hand 4)))
   (= (rank (hand 1)) (rank (hand 2)))
   (= (rank (hand 1)) (rank (hand 3)))
   (= (rank (hand 1)) (rank (hand 4)))
   (= (rank (hand 2)) (rank (hand 3)))
   (= (rank (hand 2)) (rank (hand 4)))
   (= (rank (hand 3)) (rank (hand 4))))
  )

(defn three-of-a-kind? [hand]
  (or
   (= (rank (hand 0)) (rank (hand 1)) (rank (hand 2)))
   (= (rank (hand 0)) (rank (hand 1)) (rank (hand 3)))
   (= (rank (hand 0)) (rank (hand 1)) (rank (hand 4)))
   (= (rank (hand 0)) (rank (hand 2)) (rank (hand 3)))
   (= (rank (hand 0)) (rank (hand 3)) (rank (hand 4)))
   (= (rank (hand 1)) (rank (hand 2)) (rank (hand 3)))
   (= (rank (hand 1)) (rank (hand 2)) (rank (hand 4)))
   (= (rank (hand 1)) (rank (hand 3)) (rank (hand 4)))
   (= (rank (hand 2)) (rank (hand 3)) (rank (hand 4))))
  )

(defn four-of-a-kind? [hand]
  (or
   (= (rank (hand 0)) (rank (hand 1)) (rank (hand 2)) (rank (hand 3)))
   (= (rank (hand 0)) (rank (hand 1)) (rank (hand 2)) (rank (hand 4)))
   (= (rank (hand 0)) (rank (hand 1)) (rank (hand 3)) (rank (hand 4)))
   (= (rank (hand 0)) (rank (hand 2)) (rank (hand 3)) (rank (hand 4)))
   (= (rank (hand 1)) (rank (hand 2)) (rank (hand 3)) (rank (hand 4))))
  )

(defn flush? [hand]
  (= (suit (hand 0)) (suit (hand 1)) (suit (hand 2)) (suit (hand 3)) (suit (hand 4)))
  )

(defn full-house? [hand]
  (def sorted
    (vec (sort [(rank (hand 0)) (rank (hand 1)) (rank (hand 2)) (rank (hand 3)) (rank (hand 4))])))
  (or
   (and
    (= (sorted 0) (sorted 1))
    (= (sorted 2) (sorted 3) (sorted 4)))
   (and
    (= (sorted 0) (sorted 1) (sorted 2))
    (= (sorted 3) (sorted 4))))
  )

(defn two-pairs? [hand]
  (def sorted
    (vec (sort [(rank (hand 0)) (rank (hand 1)) (rank (hand 2)) (rank (hand 3)) (rank (hand 4))])))
  (or
   (and
    (= (sorted 0) (sorted 1))
    (= (sorted 2) (sorted 3)))
   (and
    (= (sorted 0) (sorted 1))
    (= (sorted 2) (sorted 4)))
   (and
    (= (sorted 0) (sorted 1))
    (= (sorted 3) (sorted 4)))
   (and
    (= (sorted 0) (sorted 2))
    (= (sorted 3) (sorted 4)))
   (and
    (= (sorted 1) (sorted 2))
    (= (sorted 3) (sorted 4))))
  )

(defn straight? [hand]
  (def sorted1
    (vec (sort [(rank (hand 0)) (rank (hand 1)) (rank (hand 2)) (rank (hand 3)) (rank (hand 4))])))
  (def sorted2
    (vec (sort (replace {14 1} (vector (rank (hand 0)) (rank (hand 1)) (rank (hand 2)) (rank (hand 3)) (rank (hand 4)))))))
  (or
   (and
    (= (+ (sorted1 0) 1) (sorted1 1))
    (= (+ (sorted1 1) 1) (sorted1 2))
    (= (+ (sorted1 2) 1) (sorted1 3))
    (= (+ (sorted1 3) 1) (sorted1 4)))
   (and
    (= (+ (sorted2 0) 1) (sorted2 1))
    (= (+ (sorted2 1) 1) (sorted2 2))
    (= (+ (sorted2 2) 1) (sorted2 3))
    (= (+ (sorted2 3) 1) (sorted2 4))))
  )

(defn straight-flush? [hand]
  (and
   (straight? hand)
   (flush? hand))
  )

(defn value [hand]
  (cond
    (straight-flush? hand)
    8
    (four-of-a-kind? hand)
    7
    (full-house? hand)
    6
    (flush? hand)
    5
    (straight? hand)
    4
    (three-of-a-kind? hand)
    3
    (two-pairs? hand)
    2
    (pair? hand)
    1
    :else
    0)
  )
