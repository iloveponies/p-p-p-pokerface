(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rnk _] card]
    (cond
     (Character/isDigit rnk) (Integer/valueOf (str rnk))
     :else (get {\A 14, \K 13, \Q 12, \J 11, \T 10} rnk))
  )

  )


(defn suit [card]
  (let [[_ snd] card]

  (str snd))
  )

(defn pair? [hand]
  (if
    (= 2 (apply max (vals (frequencies (map rank hand))))) true false)
  )

(defn three-of-a-kind? [hand]
  (if
    (= 3 (apply max (vals (frequencies (map rank hand))))) true false)
)

(defn four-of-a-kind? [hand]
  (if
    (= 4 (apply max (vals (frequencies (map rank hand))))) true false)
)

(defn flush? [hand]
  (if (= 5 (apply max (vals (frequencies (map suit hand))))) true false))

(defn full-house? [hand]
  (= [3 2] (vals (frequencies (map rank hand)))))

(defn two-pairs? [hand]
  (= [2 2 1] (vals (frequencies (map rank hand)))))

(defn straight? [hand]
  (let [h (sort (map rank hand))
        h2 (sort (replace {14 1} h))
        [low] h
        ]
    (or (= h (range low (+ low 5)))
        (= h2 (range 1 6)))

  ))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (cond
   (two-pairs? hand) 2
   (pair? hand) 1


   (four-of-a-kind? hand) 7

   (straight-flush? hand) 8
   (straight? hand) 4
   (flush? hand) 5
   (full-house? hand) 6
   (three-of-a-kind? hand) 3
   :else 0
   )
  )

