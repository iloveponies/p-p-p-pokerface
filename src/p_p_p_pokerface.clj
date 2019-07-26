(ns p-p-p-pokerface)

(defn rank [card]
  (def replacements {\T 10, \J 11 \Q 12 \K 13 \A 14})

  (let [[first _] card]
    (if (Character/isDigit first)
      (Integer/valueOf (str first))
      (Integer/valueOf (str (replacements first))))
    ))

(defn suit [card]
  (let [[_ second] card]
     (str second)
    ))

(defn pair? [hand]
  (> (apply max (vals (frequencies (map rank hand)))) 1)
  )


(defn three-of-a-kind? [hand]
  (>= (first (vals (frequencies (map rank hand)))) 3))


(defn four-of-a-kind? [hand]
  (>= (first (vals (frequencies (map rank hand)))) 4))

(defn flush? [hand]
   (== (first (vals (frequencies (map suit hand)))) 5))


(defn full-house? [hand]
  (let [values (vals (frequencies (map rank hand)))]
    (and (== (count values) 2)  (or (== (first values) 3) (== (second values) 3) ))

    )

  )


(defn two-pairs? [hand]
  (let [values (vals (frequencies (map rank hand)))]
    (if (not (four-of-a-kind? hand))
      (and (== (first values) 2 ) (== (second values) 2 ) )
      true
      )
    )
  )


(defn straight? [hand]
    (let [sortattu (sort (map rank hand)) sortattutoisin ( sort (replace {14 1} (map rank hand)))]


        (if (= (= sortattu (range (first sortattu) (+ (first sortattu ) 5) )) false )
          (= sortattutoisin (range (first sortattutoisin) (+ (first sortattutoisin ) 5) ))
          (= sortattu (range (first sortattu) (+ (first sortattu ) 5) ))))
)


(defn straight-flush? [hand]
  (and (flush? hand) (straight? hand)))



(defn high-card? [hand]
  true) ;

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}]

     (apply max (map (fn [[checker points]] (if (checker hand) points 0) ) checkers))

    )



  )



