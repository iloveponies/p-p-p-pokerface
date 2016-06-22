(ns p-p-p-pokerface)

(defn suit [card]
  (let [[rank scnd] card]
    (str scnd)))

(defn rank [card]
  (let [[rank suit] card]
    (get {\2 2 \3 3 \4 4 \5 5 \6 6 \7 7 \8 8 \9 9 \T 10 \J 11 \Q 12 \K 13 \A 14} rank)))

(defn high-card? [hand]
  true)

(defn pair? [hand]
  (if (==
        (- (count (map rank hand))
           (count (set (map rank hand))))
        1)
  true
  false))

(defn three-of-a-kind? [hand]
  (let [sorted-hand (into [] (sort (map rank hand))) getz (fn [i] (get sorted-hand i))]
    (if (or (== (getz 0) (getz 1) (getz 2))
            (== (getz 1) (getz 2) (getz 3))
            (== (getz 2) (getz 3) (getz 4)))
      true
      false)))


(defn four-of-a-kind? [hand]
  (let [sorted-hand (into [] (sort (map rank hand))) getz (fn [i] (get sorted-hand i))]
    (if (or (== (getz 0) (getz 1) (getz 2) (getz 3))
            (== (getz 1) (getz 2) (getz 3) (getz 4)))
      true
      false)))

(defn flush? [hand]
  (let [suits (map suit hand)]
    (if (apply = suits)
      true
      false)))

(defn full-house? [hand]
  (if (and (not (four-of-a-kind? hand)) (== (count (set (map rank hand))) 2))
    true
    false))

(defn two-pairs? [hand]
  (let [sorted-hand (into [] (sort (map rank hand))) getz (fn [i] (get sorted-hand i))]
    (if (or (and (== (getz 0) (getz 1)) (== (getz 2) (getz 3)))
            (and (== (getz 0) (getz 1)) (== (getz 3) (getz 4)))
            (and (== (getz 1) (getz 2)) (== (getz 3) (getz 4))))
      true
      false)))

(defn straight? [hand]
  (let [sorted-hand (into [] (sort (map rank hand))) erotus (- (get sorted-hand 4) (get sorted-hand 0))]
    (if (and (or (== erotus 4) (== erotus 12))
             (apply < sorted-hand))
      true
      false)))

(defn straight-flush? [hand]
  (if (and (flush? hand) (straight? hand))
    true
    false))

(defn value [hand]
  (if (straight-flush? hand)
    8
    (if (four-of-a-kind? hand)
      7
      (if (full-house? hand)
        6
        (if (flush? hand)
          5
          (if (straight? hand)
            4
            (if (three-of-a-kind? hand)
              3
              (if (two-pairs? hand)
                2
                (if (pair? hand)
                  1
                  0)))))))))
