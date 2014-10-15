(ns p-p-p-pokerface)

(defn rank [card]

  (let

    [r (get card 0)
     mp {\T 10, \J 11, \Q 12, \K 13, \A 14}
     ]

    (if (Character/isDigit r)
      (Integer/valueOf (str r))
      (get mp r)
      )

    )

  )


(defn suit [card]

  (str (get card 1))

  )



(defn pair? [hand]

  (let
    [freq (frequencies (map rank hand))

     occurs (vals freq)
     maximum (apply max occurs)

     ]
    (== 2 maximum)
    )


  )


(defn three-of-a-kind? [hand]

  (let

    [freq (frequencies (map rank hand))

     occurs (vals freq)
     maximum (apply max occurs)

     ]

    (and (== 3 maximum) (== 3 (count occurs)))
    )

  )




(defn four-of-a-kind? [hand]

  (let

    [freq (frequencies (map rank hand))

     occurs (vals freq)
     maximum (apply max occurs)
     ]
     (== 4 maximum)

    )

  )

(defn straight? [hand]

  (let
    [sorted (sort (map rank hand))
    repl (sort (replace {14 1} sorted))
     ordered (range (first sorted) (+ 5 (first sorted) ))
     ordereds (range (first repl) (+ 5 (first repl) ))

     ]

    (or (= sorted ordered) (= repl ordereds))
    )

  )

(defn flush? [hand]

  (let

    [freq (frequencies (map suit hand))
     occurs (vals freq)
     cnt (count occurs)

     ]

    (and (== cnt 1) (not (straight? hand)))

  )
)


(defn full-house? [hand]
  (let
    [
     freq (frequencies (map rank hand))
     occurs (vals freq)
     cnt (count occurs)
     maximum (apply max occurs)
     ]
    (and (== 2 cnt) (== 3 maximum))
    )
  )


(defn two-pairs? [hand]

  (let
    [
     freq (frequencies (map rank hand))
     occurs (vals freq)
     cnt (count occurs)
     maximum (apply max occurs)
     ]
    (or (== 4 maximum) (and (== 2 maximum) (== 3 cnt)) )
    )

  )






(defn straight-flush? [hand]

  (let

    [freq (frequencies (map suit hand))
     occurs (vals freq)
     cnt (count occurs)
     ]


  (and (== 1 cnt) (straight? hand))
  )
 )

(defn value [hand]


  (cond
   (and (straight? hand) (not (straight-flush? hand))) 4
   (straight-flush? hand) 8
   (four-of-a-kind? hand) 7
   (full-house? hand) 6
   (flush? hand) 5

   (three-of-a-kind? hand) 3
   (two-pairs? hand) 2
   (pair? hand) 1
   :else
   0
   )

  )

