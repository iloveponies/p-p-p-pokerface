(ns p-p-p-pokerface)

(defn rank [card]
  (let [[fst _] card] 
  (get {\2 2, \3 3, \4 4, \5 5, \6 6, \7 7, \8 8, \9 9, \T 10, \J 11, \Q 12, \K 13, \A 14} fst))
)

(defn suit [card]
  (let [[_ snd] card] (str snd))
)

(defn freq [hand]
(vals (frequencies (map (fn [x] (rank x)) hand)))
)

(defn pair? [hand]
(<= 2 (apply max (freq hand)))
)

(defn three-of-a-kind? [hand]
(<= 3 (apply max (freq hand)))
)

(defn four-of-a-kind? [hand]
(<= 4 (apply max (freq hand)))
)

(defn flush? [hand]
(= 5 (apply max(vals (frequencies (map (fn [x] (suit x)) hand)))))
)

(defn full-house? [hand]
(and (= (first(freq hand)) 3) (= (last (freq hand)) 2))
)

(defn two-pairs? [hand]
(and (<= (first(freq hand)) 2) (= (first (rest (freq hand))) 2))
)

(defn straight? [hand]
(or (= (sort (map (fn [x] (rank x)) hand)) [2,3,4,5,14]) (= (sort (map (fn [x] (rank x)) hand)) (range (apply min (map (fn [x] (rank x)) hand)) (+ (apply min (map (fn [x] (rank x)) hand)) 5))))
)

(defn straight-flush? [hand]
(and (flush? hand) (straight? hand))
)

(defn high-card? [hand] true)

(defn value [hand]
(let [handvalue (cond 
(straight-flush? hand) 8 
(four-of-a-kind? hand) 7
(full-house? hand) 6
(flush? hand) 5
(straight? hand) 4
(three-of-a-kind? hand) 3
(two-pairs? hand) 2
(pair? hand) 1
:else 0)] (Integer/valueOf handvalue))
)
