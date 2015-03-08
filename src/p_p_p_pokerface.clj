(ns p-p-p-pokerface)
(defn high-card? [hand]
  true)

(defn rank [card]
   (let [a (let [
        [fst _] card]
        fst) b (get
        {\T 10, \J 11,
         \Q 12,  \K 13,
         \A 14} a )  ]
        (if (Character/isDigit a)
        (Integer/valueOf
        (str a)) b )  ))


(defn suit [card]
  (let [[_ snd] card]
  (str snd)))

(defn pair? [hand]
  (let [m (map rank hand)
   f (vals (frequencies m))
   mx (apply max f)]
  (>= mx 2) ))

(defn three-of-a-kind? [hand]
  (let [m (map rank hand) f
  (vals (frequencies m))
   mx (apply max f)]
   (>= mx 3)))

(defn four-of-a-kind? [hand]
  (let [m (map rank hand) f
  (vals (frequencies m)) mx
  (apply max f)](>= mx 4) ))

(defn flush? [hand]
  (let [ m (map suit hand) fr
  (vals (frequencies m)) mx
  (apply max fr) ](= mx 5)))

(defn full-house? [hand]
  (let [m (map rank hand) f
  (vals (frequencies m))
  st (sort f)] (= [2 3] st)))

(defn two-pairs? [hand]
  (let [m (map rank hand) f
  (vals (frequencies m)) st
  (sort f)] (or (= [1 2 2] st)
  (= [1 4] st) ) ))

(defn straight? [hand]
  (let [m (map rank hand) st
  (sort m) v (vals (frequencies st)) mx
  (apply max v)  g (apply min m) p
  (apply max m) kk (if (and (= mx 1)
  (or (= 4 (- p g)) (= 12 (- p g)))) true false )] kk ))

(defn straight-flush? [hand]
  (let [f (flush? hand) st
  (straight? hand) ] (and f st)))

(defn value [hand]
   (cond (straight-flush? hand) 8
   (four-of-a-kind? hand) 7
   (full-house? hand) 6
   (flush? hand) 5
   (straight? hand) 4
   (three-of-a-kind? hand) 3
   (two-pairs? hand) 2
   (pair? hand) 1
   (high-card? hand) 0
))
