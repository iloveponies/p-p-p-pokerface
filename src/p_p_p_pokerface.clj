(ns p-p-p-pokerface)

(defn rank [card]
  (let [values {\T 10,\J 11,\Q 12,\K 13,\A 14}
        [v s] card
        ]
    (if (Character/isDigit v) 
      (Integer/valueOf (str v))
      (values v))))

(defn suit [card]
  (let [[v s] card
        ]
    (str s) ))

(defn pair? [hand] 
  (if
    (empty? (filter (fn [x] (= x 2)) (vals(frequencies (map rank hand)))))
    false
    true))

(defn three-of-a-kind? [hand]
  (if
    (empty? (filter (fn [x] (= x 3)) (vals(frequencies (map rank hand)))))
    false
    true))

(defn four-of-a-kind? [hand]
  (if
    (empty? (filter (fn [x] (= x 4)) (vals(frequencies (map rank hand)))))
    false
    true))

(defn flush? [hand]
  (if
    (empty? (filter (fn [x] (= x 5)) (vals(frequencies (map suit hand)))))
    false
    true) )

 (defn full-house? [hand]
   (= (range 2 4) (sort(vals (frequencies (map rank hand))))))

(defn two-pairs? [hand]
  (let [ twos (apply + (map (fn [x] (if(= x 2 ) 1 0 )) (vals(frequencies (map rank  hand)))))
        ]
    (if (= twos 2) true false)) )

(defn straight? [hand]
  (let [hcards (sort(map rank hand))
        lcards (sort(replace {14 1} (map rank hand)))
        hmin (apply min hcards) 
        hmax (+ 1 (apply max hcards)) 
        lmin (apply min lcards) 
        lmax (+ 1 (apply max lcards)) 
        ]
    (or
      (= hcards (range hmin hmax))
      (= lcards (range lmin lmax)))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand))
  )

(defn value [hand]
  (cond 
    (straight-flush? hand) 8
    (four-of-a-kind? hand) 7
    (full-house? hand) 6
    (flush?  hand) 5
    (straight? hand) 4
    (three-of-a-kind? hand) 3
    (two-pairs? hand) 2
    (pair? hand) 1
    :else 0
    )
  )
