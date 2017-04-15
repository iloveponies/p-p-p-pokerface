(ns p-p-p-pokerface)

(def rep {\A 14, \K 13, \Q 12, \J 11, \T 10})

(defn rank [card] (if (= (Character/isDigit (let [[sd] card]
  sd) ) true) (Integer/valueOf (str (let [[sd] card]
  sd) )) (rep (let [[sd] card]
  sd)) ))


(defn su [x] (let [[_ snd] x]
  snd))

(defn suit [card] (str (su card)))


(defn pair? [hand] (not= (count (vec (map rank hand))) (count (vec (keys (frequencies (vec (map rank hand))))))))


(defn three-of-a-kind? [hand] (= (apply max (vec (vals (frequencies (vec (map rank hand)))))) 3))


(defn four-of-a-kind? [hand] (= (apply max (vec (vals (frequencies (vec (map rank hand)))))) 4))


(defn flush? [hand] (= (vec (vals (frequencies (vec (map suit hand))))) [5]))


(defn full-house? [hand] (= (vec (sort (vec (vals (frequencies (vec (map rank hand))))))) [2 3]))


(defn two-pairs? [hand]
  (or (=
       (vec (vals (frequencies (vec (map rank hand))))) [2 2 1])
         (= (vec (vals (frequencies (vec (map rank hand))))) [4 1])) )


(defn straight? [hand]
  (or
   (= (sort (vec (map rank hand))) [2 3 4 5 14])
      (= (sort (vec (map rank hand))) [10 11 12 13 14])
   (= (sort (vec (map rank hand))) [2 3 4 5 6])) )


(defn straight-flush? [hand] (and (straight? hand) (flush? hand)))


(defn value [hand]
  nil)
