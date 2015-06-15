(ns p-p-p-pokerface)

(defn rank [card]
  (let [[fst _] card]
    (if (Character/isDigit fst)
      (Integer/valueOf (str fst))
      (if (= false (Character/isDigit fst))
        (cond
         (= fst \T) 10
         (= fst \J) 11
         (= fst \Q) 12
         (= fst \K) 13
         (= fst \A) 14)))))


(defn suit [card]
  (let [[_ snd] card]
    (str snd)))


(defn pair? [hand]
  (let [first (rank (get hand 0))
        second (rank (get hand 1))
        third (rank (get hand 2))
        fourth (rank (get hand 3))
        fifth (rank (get hand 4))]
    (if (= 5 (count (vals (frequencies [first second third fourth fifth]))))
      false
      true)))


(defn three-of-a-kind? [hand]
  (let [first (rank (get hand 0))
        second (rank (get hand 1))
        third (rank (get hand 2))
        fourth (rank (get hand 3))
        fifth (rank (get hand 4))]
    (let [setti (vals (frequencies [first second third fourth fifth]))]
      (let [maksimi (apply max setti)]
        (cond
         (= 3 maksimi) true
         (= 4 maksimi) true
         (= 5 maksimi) true
         :else false)))))


(defn four-of-a-kind? [hand]
  (let [first (rank (get hand 0))
        second (rank (get hand 1))
        third (rank (get hand 2))
        fourth (rank (get hand 3))
        fifth (rank (get hand 4))]
    (let [setti (vals (frequencies [first second third fourth fifth]))]
      (let [maksimi (apply max setti)]
        (cond
         (= 4 maksimi) true
         (= 5 maksimi) true
         :else false)))))


(defn flush? [hand]
  (let [first (suit (get hand 0))
        second (suit (get hand 1))
        third (suit (get hand 2))
        fourth (suit (get hand 3))
        fifth (suit (get hand 4))]
      (if (= 1 (count (vals (frequencies [first second third fourth fifth]))))
        true
        false)))


(defn full-house? [hand]
  (let [first (rank (get hand 0))
        second (rank (get hand 1))
        third (rank (get hand 2))
        fourth (rank (get hand 3))
        fifth (rank (get hand 4))]
      (let [setti (sort (vals (frequencies [first second third fourth fifth])))]
        (if (= setti [2 3])
          true
          false))))


(defn two-pairs? [hand]
  (let [first (rank (get hand 0))
        second (rank (get hand 1))
        third (rank (get hand 2))
        fourth (rank (get hand 3))
        fifth (rank (get hand 4))]
    (let [setti (sort (vals (frequencies [first second third fourth fifth])))]
      (cond
       (= setti [1 2 2]) true
       (= setti [1 4]) true
       :else false))))


(defn straight? [hand]
  (let [first (rank (get hand 0))
        second (rank (get hand 1))
        third (rank (get hand 2))
        fourth (rank (get hand 3))
        fifth (rank (get hand 4))]
    (let [setti (sort [first second third fourth fifth])]
      (let [minimi (apply min setti)
            maksimi (apply max setti)]
        (let [erotus (- maksimi minimi)]
          (if (= setti [minimi (+ 1 minimi) (+ 2 minimi) (+ 3 minimi) (+ 4 minimi)])
            true
            (if (and (= 14 maksimi) (= 5 (apply max (sort (replace {14 1} setti)))))
              true
              false)))))))

(defn straight-flush? [hand]
  (if (and (straight? hand) (flush? hand))
    true
    false))


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
