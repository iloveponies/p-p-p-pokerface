(ns p-p-p-pokerface)

(def facecardnums {\T 10, \J 11, \Q 12 \K 13, \A 14})

(defn high-card? [hand]
  true)

(defn rank [card]
  (let [[fst _] card]
    (cond (Character/isDigit fst) 
      (Integer/valueOf (str fst))
    :else
      (Integer/valueOf (str (get facecardnums fst)))
    )
  )
)

(defn suit [card]
  (let [[_ snd] card]
    (str snd)
  )
)

(defn pair? [hand]
  (let [thevalues (for [x hand] (rank x))
        freaks (vals (frequencies thevalues))
        mostof (apply max freaks)
       ]
    (if (> mostof 1) true false)
  )
)

(defn three-of-a-kind? [hand]
  (let [thevalues (for [x hand] (rank x))
        freaks (vals (frequencies thevalues))
        mostof (apply max freaks)
       ]
    (if (>= mostof 3) true false)
  )
)

(defn four-of-a-kind? [hand]
  (let [thevalues (for [x hand] (rank x))
        freaks (vals (frequencies thevalues))
        mostof (apply max freaks)
       ]
    (if (>= mostof 4) true false)
  )
)

(defn flush? [hand]
  (let [thesuits (for [x hand] (suit x))
        freaks (vals (frequencies thesuits))
        mostof (apply max freaks)
       ]
    (if (>= mostof 5) true false)
  )
)

(defn full-house? [hand]
  (let [thevalues (for [x hand] (rank x))
        freaks (vals (frequencies thevalues))
        sortedfreaks (sort freaks)
       ]
    (if (= sortedfreaks (seq [2 3])) true false)
  )
)

(defn two-pairs? [hand]
  (let [thevalues (for [x hand] (rank x))
        freaks (vals (frequencies thevalues))
        sortedfreaks (sort freaks)
        fourkind (four-of-a-kind? hand)																																																																																																																																																																																																																																			
       ]
    (cond (= true fourkind) true 
      :else 
      (if (= sortedfreaks (seq [1 2 2])) true false))
  )
)

(defn straight? [hand]
  (let [thevalues (for [x hand] (rank x))
      sortedacehigh (sort thevalues)
      highacestartval (nth sortedacehigh 0)
      sortedacelow (sort (replace {14 1} thevalues))
      lowacestartval (nth sortedacelow 0)
     ]
    (cond (= sortedacehigh (range highacestartval (+ highacestartval 5))) true
     :else (= sortedacelow (range lowacestartval (+ lowacestartval 5))))
  )
)

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand))
)

(defn value [hand]
 (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}]
    (cond (straight-flush? hand) 8
    :else (cond (four-of-a-kind? hand) 7 
          :else (cond (full-house? hand) 6
                :else (cond (flush? hand) 5
                      :else (cond (straight? hand) 4
                            :else (cond (three-of-a-kind? hand) 3
                                  :else (cond (two-pairs? hand) 2
                                        :else (cond (pair? hand) 1
                                              :else (cond (high-card? hand) 0)
                                              )
                                        )
                                  )
                            )
                      )
                )
          )
    )
  )
)
