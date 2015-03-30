(ns p-p-p-pokerface)

(def replacements {	\T 10,  	\J 11 , 	\Q 12 , 	\K 13 , 	\A  14})
(replacements \T)


(defn rank [card]

   (let [[fst snd] card]


    ( if (Character/isDigit fst) (Integer/valueOf (str fst)) (replacements fst) )
  )
)

(defn suit [card]

  (let [[fst snd] card]
  (str snd) )

)


(defn pair? [hand]
  (let

      [ a  (set (map rank hand)  )
        cou (count a)
            ]
    (if (== (mod cou 2) 0) true false )
    )
)

(defn three-of-a-kind? [hand]

    (let [ second-elements (fn [collection]
            ( let [abc (fn [x] (get x 1 ))]
            (map abc collection) )
                           )
      ]

     (if (== 3 ( apply max  ( second-elements (frequencies (map rank hand))) ) ) true false)

  )
)



(defn four-of-a-kind? [hand]
(let [ second-elements (fn [collection]
            ( let [abc (fn [x] (get x 1 ))]
            (map abc collection) )
                           )
      ]

     (if (== 4 ( apply max  ( second-elements (frequencies (map rank hand))) ) ) true false)

  )

  )

(defn flush? [hand]
 (if (== 1 (count (set (map suit hand) )) ) true false)
  )

(defn full-house? [hand]
  (if (and ( three-of-a-kind? hand ) ( pair?  hand )) true false)
  )


(defn two-pairs? [hand]
  (let [ second-elements (fn [collection]
            ( let [abc (fn [x] (get x 1 ))]
            (map abc collection) )
                           )
          a (sort (second-elements (frequencies (map rank hand) ) ))
      ]


    (if (or (= a [1 2 2]) (= a [1 4]) )  true false)

  )

)



(defn straight? [hand]
  (let  [
   a (sort (map rank hand) )
      min (apply min a   )

          ]

    (if (or (= a [2 3 4 5 14] ) (= a (range min (+ min 5) ) ) ) true false)

    )
)


(defn straight-flush? [hand]

   ( if (and (straight? hand ) (flush? hand) ) true false)
  )

(defn value [hand]
   (let [checkers #{  [true  0]  [(pair? hand) 1]

                 [(two-pairs? hand) 2]  [(three-of-a-kind? hand) 3]
                 [(straight? hand) 4]   [(flush? hand) 5]
                 [(full-house? hand) 6] [(four-of-a-kind? hand) 7]
                 [(straight-flush? hand) 8]}  ]


     (apply max  (map second (filter (fn [x] (= (first x) true)) checkers ) ) )
  )
)

(defn high-card? [hand]   true)

