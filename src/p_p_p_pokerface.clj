(ns p-p-p-pokerface)

(def replacements {\T 10, \J 11, \Q 12, \K 13, \A 14})


(defn rank [card]
  (if (Character/isDigit (first card))
    (Integer/valueOf(str (first card)))
    (replacements (first card))
    ))

(defn suit [card]
  (str(second card)))

(defn pair? [hand]
 (== (apply max(vals (frequencies (map rank hand))) ) 2)
  )

(defn three-of-a-kind? [hand]
  (== (apply max(vals (frequencies (map rank hand))) ) 3))

(defn four-of-a-kind? [hand]
  (== (apply max(vals (frequencies (map rank hand))) ) 4))

(defn flush? [hand]
  (== (apply max(vals (frequencies (map suit hand))) ) 5))



(defn full-house? [hand]
  (let [vals (fn [x] (max(vals (frequencies (map rank hand))) ))]
       (and
        (== (first (vals hand)) 3)
        (== (second (vals hand)) 2)
        )
    ))

(defn two-pairs? [hand]
   (let [vals (fn [x] (max(vals (frequencies (map rank hand))) ))]
       (or
         (and
           (== (first (vals hand)) 2)
           (== (second (vals hand)) 2))
         (== (first (vals hand)) 4)
    )))



(defn straight? [hand]
  (let
    [vals (fn [] (sort(mapv rank hand)))
     ace-is-1 (fn [] (sort(replace {14 1} (vals))))]
  (or
   (=
    (vals)
    (range (first (vals)) (+ (first(vals)) 5)))
   (=
    (ace-is-1)
    (range (first (ace-is-1)) (+ (first (ace-is-1)) 5)))
    )))

(defn straight-flush? [hand]
  (and (flush? hand) (straight? hand)))

(defn high-card? [hand]
  true) ; All hands have a high card.



(defn hand-has-value? [hand value]
                 (let [checkers [high-card? pair? two-pairs? three-of-a-kind? straight?
                flush? full-house? four-of-a-kind? straight-flush?]]
                    ((get checkers value) hand)
                   )

                 )






(defn value [hand]

  (apply max (filter (fn [x] (hand-has-value? hand x)) (range 9)))


)
