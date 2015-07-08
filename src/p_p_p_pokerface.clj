(ns p-p-p-pokerface)



(def replacements {\A 14, \T 10, 
                   \J 11, \Q 12, \K 13})



(defn rank [card]
  (let [[value suit] card]
    (if (Character/isDigit value) 
      (Integer/valueOf (str value))
      (replacements value)) 
    ))



(defn suit [card]
  (let [[face suit] card]
    (str suit))
)




(defn pair? [hand]
  (let [ranks (map rank hand)
        freqs (frequencies ranks)]
    (contains?(set(vals freqs))
              2))
  )


(defn three-of-a-kind? [hand]
  (let [ranks (map rank hand)
        freqs (frequencies ranks)]
    (contains?(set(vals freqs))
              3)
    )
  )





(defn four-of-a-kind? [hand]
  (let [ranks (map rank hand)
        freqs (frequencies ranks)]
    (contains?(set(vals freqs))
              4)
    )
)


 (defn flush? [hand]
  (let [suits (map suit hand)
        freqs (frequencies suits)]
    (= 1 (count(keys freqs)))
    )
)
    


(defn full-house? [hand]
  (and (pair? hand)
       (three-of-a-kind? hand)
       )
)



(defn two-pairs? [hand]
  (let [ranks (map rank hand)
        freqs (frequencies ranks)
        pairs (filter (fn [x] (= x 2))
               (vals freqs))]
    (or 
     (four-of-a-kind? hand)
     (= 2 (count pairs))
     )
    )
)



(defn straight? [hand]
  (let [ranks (map rank hand)
        ordered (map (fn [x] (- x (first ranks))) 
                     (sort ranks)
                     )
        ]
    (or 
     (= (range 0 5)  ordered)
     (= [0 1 2 3 12] ordered)
     ))
)


(defn straight-flush? [hand]
  (and 
   (straight? hand)
   (flush? hand)
))


(defn high-card? [hand] 
true)


           ;=> 0

(defn value [hand]
  (let [evalmap {0 (high-card? hand) 
               1 (pair? hand)
               2 (two-pairs? hand)
               3 (three-of-a-kind? hand)
               4 (straight? hand)
               5 (flush? hand)
               6 (full-house? hand)
               7 (four-of-a-kind? hand)
               8 (straight-flush? hand)}
        scores (filter (fn [x] (second x)) evalmap)]
    (first (last scores))))
 
