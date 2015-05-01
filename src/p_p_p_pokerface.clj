(ns p-p-p-pokerface)
; apuri
(def ranks {\T 10, \J 11, \Q 12, \K 13, \A 14})

; EXERCISE 2
(defn rank [card]
  (let [ [fst snd] card ]
    (if (Character/isDigit fst)
        (Integer/valueOf (str fst))
        (get ranks fst))) )

;(rank "2H") ;=> 2
;(rank "4S") ;=> 4
;(rank "TS") ;=> 10
;(rank "JS") ;=> 11
;(rank "QS") ;=> 12
;(rank "KS") ;=> 13
;(rank "AS") ;=> 14

; EXERCISE 1
(defn suit [card]
  (let [ [fst snd] card ]
    (str snd))
  )

;(suit "2H") ;=> "H"
;(suit "2D") ;=> "D"
;(suit "2C") ;=> "C"
;(suit "3S") ;=> "S"


; EXERCISE 3
(defn pair? [hand]
  (= 2 (apply max (vals (frequencies (map rank hand)))))
  )


; EXERCISE 4
(defn three-of-a-kind? [hand]
  (= 3 (apply max (vals (frequencies (map rank hand)))))
  )


; EXERCISE 5
(defn four-of-a-kind? [hand]
  (= 4 (apply max (vals (frequencies (map rank hand)))))
  )


; EXERCISE 6
(defn flush? [hand]
  (= 5 (apply max (vals (frequencies (map suit hand)))))
  )


; EXERCISE 7
(defn full-house? [hand]
  (and (not (four-of-a-kind? hand))
       (= 2 (count (vals (frequencies (map rank hand))))))
  )


; EXERCISE 8
(defn two-pairs? [hand]
  (or
     (and (not (three-of-a-kind? hand) )
          (= 3 (count (vals (frequencies (map rank hand))))))
     (four-of-a-kind? hand))
  )


; EXERCISE 9
(defn straight? [hand]
  (let [hvals (map rank hand)
        hvalsb (replace {1 14, 14 1} hvals)
        ]
    (or
       (= (sort hvals) (range (apply min hvals) (+ (apply min hvals) 5)))
       (= (sort hvalsb) (range (apply min hvalsb) (+ (apply min hvalsb) 5)))))
  )


; EXERCISE 10
(defn straight-flush? [hand]
  (and
    (flush? hand)
    (straight? hand)))


; EXERCISE 11
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
        :else 0)
  )
