(ns p-p-p-pokerface)

(defn rank [card]
  (def muunna {\T 10, \J 11, \Q 12, \K 13, \A 14})
  (let [[rank _] card]
   (if (Character/isDigit rank)
     (Integer/valueOf (str rank))
     (Integer/valueOf (str (get muunna rank))))))

(defn suit [card]
  (let [[_ snd] card]
   (str snd)
  ))

(defn pair? [hand]
  (let [ranks (map rank hand)]
  	(if (=(apply max (vals (frequencies ranks))) 2) 
     true false)))

(defn three-of-a-kind? [hand]
  (let [ranks (map rank hand)]
  	(if (=(apply max (vals (frequencies ranks))) 3) 
     true false)))

(defn four-of-a-kind? [hand]
  (let [ranks (map rank hand)]
  	(if (=(apply max (vals (frequencies ranks))) 4) 
     true false)))

(defn flush? [hand]
 (let [suits (map suit hand)]
  (if (=(apply max (vals (frequencies suits))) 5) 
     true false)))

(defn full-house? [hand]
 (let [ranks (map rank hand)]
  	(if (=(seq (sort (vals (frequencies ranks)))) [2 3]) 
     true false)))

(defn two-pairs? [hand]
  (let [ranks (map rank hand)]
  	(cond
      (=(seq (sort (vals (frequencies ranks)))) [1 2 2]) true  
      (four-of-a-kind? hand) true
      :else false)))

(defn straight? [hand]
  nil)

(defn straight-flush? [hand]
  nil)

(defn value [hand]
 (cond 
  (straight-flush? hand) 8
  (four-of-a-kind? hand) 7
  (full-house? hand) 6
  (flush? hand) 5
  (straight? hand) 4
  (three-of-a-kind? hand) 3
  (two-pairs? hand) 2
  (pair? hand) 1
  :else 0))