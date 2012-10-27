(ns p-p-p-pokerface)

(defn rank [[rank evvk]]

  (def kuvakortit {\T 10 \J 11 \Q 12 \K 13 \A 14})

  (if (Character/isDigit rank)
    (Integer/valueOf (str rank))
    (get kuvakortit rank)


  ))

(defn suit [[evvk suit]]
  (str suit)
  )

(defn pair? [hand]

	(if (>= (apply max (vals (frequencies(map rank hand)))) 2)
        true
        false)


  )

(defn three-of-a-kind? [hand]
  (if (>= (apply max (vals (frequencies(map rank hand)))) 3)
        true
        false)
)

(defn four-of-a-kind? [hand]
 (if (>= (apply max (vals (frequencies(map rank hand)))) 4)
        true
        false)
)

(defn flush? [hand]

	 (if (= (apply max (vals (frequencies(map suit hand)))) 5)
       true
       false
       )

  )

(defn full-house? [hand]

(if (and (= 2 (apply min(vals (frequencies(map rank hand)))))
         (= 3 (apply max(vals (frequencies(map rank hand))))))
  true
  false


  )
  )

(defn two-pairs? [hand]

(if (<= 4 (apply + (filter (fn [x] (>= x 2)) (vals (frequencies(map rank hand))))))
  true
  false)

)

(defn straight? [hand]


  (def kasi  (sort (map rank hand)))

  (if (= 2 (first kasi))
	(if (= (range 1 6) (sort (replace {14 1} kasi )))
        true
		(if(= (range 2 7) kasi)
			true
          	false
          )

      )


   (if (= (range (first kasi) (+ (first kasi) 5)) kasi)
       true
        false)


  )

  )

(defn straight-flush? [hand]


  (if (and (straight? hand) (flush? hand))
    true
    false)


  )

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
