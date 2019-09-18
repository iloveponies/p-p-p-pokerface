(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rank suit] card
        replacements {\T 10, \J 11, \Q 12, \K 13, \A 14}]
    (if (Character/isDigit rank)
     (Integer/valueOf (str rank))
     (replacements rank))))


(defn suit [card]
  (let [[rank suit] card]
     (str suit)
  ))

(defn pair? [hand]
  (let [handf (vals (frequencies (map (fn [x] (rank x)) hand)))]
    (>= (count (filter (fn [x] (= 2 x)) handf)) 1)
    ))

(defn three-of-a-kind? [hand]
    (let [handf (vals (frequencies (map (fn [x] (rank x)) hand)))]
       (>= (count (filter (fn [x] (= 3 x)) handf)) 1)))


(defn four-of-a-kind? [hand]
    (let [handf (vals (frequencies (map (fn [x] (rank x)) hand)))]
    (>= (count (filter (fn [x] (= 4 x)) handf)) 1)))

(defn flush? [hand]
  (let [suithand (map (fn [x] (suit x)) hand)
        suitf (frequencies suithand)]
     (apply = 5 (vals suitf))))

(defn full-house? [hand]
  (let [handf  (vals (frequencies (map (fn [x] (rank x)) hand)))
        [a b] handf]
   (and (= (count handf) 2)
        (and (or (= 3 a) (= 2 a))
             (or (= 3 b) (= 2 b))))))

(defn two-pairs? [hand]
    (let [handf (vals (frequencies (map (fn [x] (rank x)) hand)))]
    (>= (count (filter (fn [x] (= 2 x)) handf)) 2)

    ))
(defn alt-rank [card]
    (let [[rank suit] card
          replacements {\T 10, \J 11, \Q 12, \K 13, \A 1}]
    (if (Character/isDigit rank)
     (Integer/valueOf (str rank))
     (replacements rank))))

(defn straight? [hand]
    (let [handrank (map (fn [x] (rank x)) hand)
          alt-hand (map (fn [x] (alt-rank x)) hand )
          maximum (apply max handrank)
          minimum (apply min handrank)
          alt-min (apply min alt-hand)
          alt-max (apply max alt-hand)]

         (or  (= (apply + alt-hand) (/ (* alt-max (+ alt-max 1)) 2)) (= (apply + handrank) (/(*(-(+ maximum 1 ) minimum) (+ minimum maximum))2))))
    )

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand))
  )

(defn value [hand]
  (cond
   (four-of-a-kind? hand) 7
   (straight-flush? hand) 8
   (straight? hand) 4
   (flush? hand) 5
   (full-house? hand) 6
   (three-of-a-kind? hand) 3
   (two-pairs? hand) 2
   (pair? hand)  1
   :else 0
  )
)
