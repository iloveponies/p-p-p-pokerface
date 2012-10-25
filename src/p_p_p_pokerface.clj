(ns p-p-p-pokerface)

(defn rank
  [card]
  (let [[f s] (str card)]
   (get    
   {\1 1
   \2 2
   \3 3
   \4 4
   \5 5
   \6 6
   \7 7
   \8 8
   \9 9
   \T 10
   \J 11
   \Q 12
   \K 13
   \A 14} 
   f)))

(defn suit [card]
  (str (get card 1)))

(defn pair? [hand]
  (if (> (apply max (vals (frequencies (map rank hand)))) 1) true false))

(defn three-of-a-kind? [hand]
  (if (> (apply max (vals (frequencies (map rank hand)))) 2) true false))

(defn four-of-a-kind? [hand]
  (if (> (apply max (vals (frequencies (map rank hand)))) 3) true false))

(defn flush? [hand]
  (cond 
  (= (get (frequencies (map suit hand)) "H") 5) true
  (= (get (frequencies (map suit hand)) "D") 5) true
  (= (get (frequencies (map suit hand)) "S") 5) true
  (= (get (frequencies (map suit hand)) "C") 5) true
  :else false))

(defn full-house? [hand]
  (cond
  (and (== (apply max (vals (frequencies (map rank hand)))) 3)
       (== (apply min (vals (frequencies (map rank hand)))) 2)) true
  :else false))

(defn two-pairs? [hand]
  (if (or (= [2 2 1] (vals (frequencies (map rank hand))))
	  (= [2 1 2] (vals (frequencies (map rank hand))))
      (= [1 2 2] (vals (frequencies (map rank hand))))
      (= [4 1] (vals (frequencies (map rank hand))))
      (= [1 4] (vals (frequencies (map rank hand))))
      (= [5] (vals (frequencies (map rank hand))))
      ) true false))

(defn straight? [hand]
  (let [arvot (sort (map rank hand))
	    minimi (apply min (map rank hand))
        maksimi (apply max (map rank hand))
        ero (range minimi (inc maksimi))
        pituus 5]
    	(cond
     	(and (= ero arvot)
         	 (= (count ero) pituus)) true
        (= arvot [2 3 4 5 14]) true
        :else false)
    ))

(defn straight-flush? [hand]
  (and (straight? hand)(flush? hand)))

(defn value [hand]
  (let [tulos 0]
  (cond
	(straight-flush? hand) (+ tulos 8)
    (four-of-a-kind? hand) (+ tulos 7)
    (full-house? hand) (+ tulos 6)
    (flush? hand) (+ tulos 5)
    (straight? hand) (+ tulos 4)
    (three-of-a-kind? hand) (+ tulos 3)
    (two-pairs? hand) (+ tulos 2)
    (pair? hand) (+ tulos 1)
    :else tulos
   )))