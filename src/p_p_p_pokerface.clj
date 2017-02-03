(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rank _] card]
    (cond
      (= rank \T)  10
      (= rank \J)  11
      (= rank \Q)  12
      (= rank \K)  13
      (= rank \A)  14
      :else       (Integer/valueOf (str rank))
    )
  )
)

(defn suit [card]
  (let [[_ suit] card]
    (str suit)
  )
)


(defn pair? [hand]
  (= 1 (count (filter (fn [x] (= x 2))  (vals (frequencies (map rank hand))))))
)

(defn three-of-a-kind? [hand]
  (let [valores  (vals (frequencies (map rank hand)))]
    (and (= (apply max valores) 3) (= (apply min valores) 1))
  )
)

(defn four-of-a-kind? [hand]
   (= (apply max (vals (frequencies (map rank hand)))) 4)
)

(defn flush? [hand]
   (apply = (map suit hand))
)

(defn full-house? [hand]
  (let [valores  (vals (frequencies (map rank hand)))]
    (and (= (apply max valores) 3) (= (apply min valores) 2))))

(defn two-pairs? [hand]
  (= 2 (count (filter (fn [x] (= x 2))  (vals (frequencies (map rank hand))))))
)

(defn straight? [hand]
  (let [numeros (map rank hand)
        nuevos_numeros (replace {14,1} numeros)
        frecuencias (vals (frequencies numeros))]
        (and (= (apply max frecuencias) 1)
             (or
                (= (- (apply max numeros) (apply min numeros)) 4)
                (= (- (apply max nuevos_numeros) (apply min nuevos_numeros)) 4)
             )
        )
   )
)


(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  true
)

;(defn value [hand]
;  (let [checkers #{[high-card? 0]  [pair? 1]
;                   [two-pairs? 2]  [three-of-a-kind? 3]
;                   [straight? 4]   [flush? 5]
;                   [full-house? 6] [four-of-a-kind? 7]
;                   [straight-flush? 8]
;                   }
;       ]
;    (map (fn [x] (if (apply (first x) hand) (second x))) checkers)
;   )
;)

(defn value [hand]
  (cond
     (straight-flush? hand) 8
     (four-of-a-kind? hand) 7
     (full-house? hand)     6
     (flush? hand)          5
     (straight? hand)       4
     (three-of-a-kind? hand)3
     (two-pairs? hand)      2
     (pair? hand)           1
     :else                  0

  )
)
