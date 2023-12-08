(ns p-p-p-pokerface)

(defn rank [card]
(let [ranking {\T 10, \J 11, \Q 12, \K 13, \A 14},
  [first _] card]
    (Integer/valueOf (if (Character/isDigit first) (str first)
      (str(ranking first))
      ))
  ))

(defn suit [card]
  (let [[_ snd] card]
  (str snd)))

(defn pair? [hand]
  (>= (apply max (vals (frequencies (map rank hand)))) 2)
  )

(defn three-of-a-kind? [hand]
 (>= (apply max (vals (frequencies (map rank hand)))) 3)
  )

(defn four-of-a-kind? [hand]
 (>= (apply max (vals (frequencies (map rank hand)))) 4)
  )

(defn flush? [hand]
(apply = (map second hand))
  ;(= (first (vals (frequencies(apply str (map second hand)))))5)
  )

(defn full-house? [hand]
(= (sort (vals (frequencies (map first hand)))) [2 3])
  )

(defn two-pairs? [hand]
(or (= (sort (vals (frequencies (map first hand)))) (seq [1 2 2])) (= (sort (vals (frequencies (map first hand)))) (seq [1 4])))
  ;(sort (vals (frequencies (map first hand))))
  )

(defn straight? [hand]
  (let [[ x1 x2 x3 x4 x5]
  (sort (map rank hand)),
    [ y1 y2 y3 y4 y5]
  (sort (replace {14 1} (map rank hand)))
        ]

    (or (= (- x2 x1) (- x3 x2) (- x4 x3) (- x5 x4))
        (= (- y2 y1) (- y3 y2) (- y4 y3) (- y5 y4))
        )
    )
  )

(defn straight-flush? [hand]

  (and (straight? hand) (flush? hand))
  )

(defn value [hand]
  (case (straight-flush? hand) true 8
    (case (four-of-a-kind? hand) true 7
      (case (full-house? hand) true 6
        (case (flush? hand) true 5
          (case (straight? hand) true 4
            (case (three-of-a-kind? hand) true 3
              (case (two-pairs? hand) true 2
                (case (pair? hand) true 1
    0
    )

  ))))))))
