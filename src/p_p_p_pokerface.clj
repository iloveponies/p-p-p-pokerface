(ns p-p-p-pokerface)

(defn rank [card]
  (let [[fst _] card]
    (if (Character/isDigit fst)
      (Integer/valueOf (str fst))
      ({\T 10, \J 11, \Q 12, \K 13, \A 14} fst))))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn pair? [hand]
  (let [[fst snd trd fth ffh] hand]
    (>= (apply max (vals (frequencies 
     [(rank fst) (rank snd) (rank trd) (rank fth) (rank ffh)]))) 2)))

(defn three-of-a-kind? [hand]
  (let [[fst snd trd fth ffh] hand]
    (>= (apply max (vals (frequencies 
     [(rank fst) (rank snd) (rank trd) (rank fth) (rank ffh)]))) 3)))

(defn four-of-a-kind? [hand]
  (let [[fst snd trd fth ffh] hand]
    (>= (apply max (vals (frequencies 
     [(rank fst) (rank snd) (rank trd) (rank fth) (rank ffh)]))) 4)))

(defn flush? [hand]
  (let [[fst snd trd fth ffh] hand]
    (= (suit fst) (suit snd) (suit trd) (suit fth) (suit ffh))))

(defn full-house? [hand]
  (let [[fst snd trd fth ffh] hand]
    (and 
     (== (apply min (vals (frequencies 
      [(rank fst) (rank snd) (rank trd) (rank fth) (rank ffh)]))) 2)
     (== (apply max (vals (frequencies 
      [(rank fst) (rank snd) (rank trd) (rank fth) (rank ffh)]))) 3))))

(defn two-pairs? [hand]
  (let [[x y] (sort (vals (frequencies (map rank hand))))]
   (< 1 y)))

(defn straight? [hand]
  (if (pair? hand)	
   false
    (let [[fst snd trd fth ffh] (sort (map rank hand))]
     (if (== 4 (+ (- ffh fth) (- fth trd) (- trd snd) (- snd fst)))
      true
     (if (== 4 (+ (- fth trd) (- trd snd) (- snd fst) (- fst (- ffh  13))))
      true
      false)))))

(defn straight-flush? [hand]
  (and (flush? hand) (straight? hand)))

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