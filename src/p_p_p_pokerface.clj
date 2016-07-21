(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rank _] card]
    (if (Character/isDigit rank)
      (Integer/valueOf (str rank))
      (get {\T 10 \J 11 \Q 12 \K 13 \A 14} rank)
      )
    )
  )

(defn suit [card]
  (let [[_ suit] card]
  (str suit))
)

(defn pair? [hand]
  (let [ result
	(filter (fn [x] (= x 2))
	(vals (frequencies 
	 (map rank hand)
	 )
	      )
	)]
  
   (not (empty? result) )
    )
  )

(defn three-of-a-kind? [hand]
    (let [ result
	(filter (fn [x] (= x 3))
	(vals (frequencies 
	 (map rank hand)
	 )
	      )
	)]
  
   (not (empty? result) )
    )

  )

(defn four-of-a-kind? [hand]
      (let [ result
	(filter (fn [x] (= x 4))
	(vals (frequencies 
	 (map rank hand)
	 )
	      )
	)]
  
   (not (empty? result) )
    )

  )

(defn flush? [hand]
 (= 5  (apply max (vals (frequencies (map suit hand)))))
  ;; (let [ranks (map rank hand)]
  ;;   (cond 
  ;;    (not (= (- (max ranks) (min ranks)) 0)) false
  ;;    (> (max  (frequencies ranks)) 1) false
  ;;    :else true	 
  ;;  )
  ;; )
  )

(defn full-house? [hand]
  (let [freq (sort (vals (frequencies (map rank hand))))
	]
    (=  [2 3] (seq freq   ))
    )
  )

(defn two-pairs? [hand]
;;   (= [2 2 ]  (filter (fn [x] (= x 2))  (sort (vals (frequencies (map rank hand))))))

  (let [freq (sort (vals (frequencies (map rank hand))))
;	pairs (filter (fn [x] (= x 2) ) freq)
	]
    (=  [2 2]   (filter (fn [x] (= x 2) ) freq))
    )  
  )

(defn straight? [hand]
  (let [ ranks (sort (map rank hand))
	ranks-alter (sort (replace {14 1}  (map rank hand)))
;;	rank-seq (range (min ranks) (max ranks)
	]
    (cond
     (= (range (apply min ranks) (inc (apply max ranks))) ranks) true
     (= (range (apply min ranks-alter) (inc (apply max ranks-alter))) ranks-alter) true
     :else false
     )
    )  
  )

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand))
  )

(defn value [hand]
  (cond
   (straight-flush? hand) 8
   (four-of-a-kind? hand) 7
   (full-house? hand ) 6
   (flush? hand) 5
   (straight? hand) 4
   (three-of-a-kind? hand) 3
   (two-pairs? hand) 2
   (pair? hand) 1
   :else 0
   )
  )
