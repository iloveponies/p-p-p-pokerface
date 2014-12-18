(ns p-p-p-pokerface)

(def pic-cards { \T 10, \J 11, \Q 12, \K 13, \A 14 })

(defn rank [card]
  (let [[fst _] card]
	(if (Character/isDigit fst) 
		(Integer/valueOf (str fst))
		(get pic-cards fst))))

(defn suit [card]
  (let [[_ snd] card] (str snd)))

(defn freq-vals [rank-or-suit hand]
	(let [f (frequencies (map rank-or-suit hand))]
		(vals f)))
			
(defn n-of-a-kind? [hand n]
  (>= (apply max (freq-vals rank hand)) n))

(defn pair? [hand]
	(n-of-a-kind? hand 2))

(defn three-of-a-kind? [hand]
	(n-of-a-kind? hand 3))

(defn four-of-a-kind? [hand]
	(n-of-a-kind? hand 3))

(defn flush? [hand]
  (= (apply max (freq-vals suit hand)) 5))

(defn full-house? [hand]
  	(let [fv (freq-vals rank hand)]
  		(= (range 2 4) (sort fv))))

(defn two-pairs? [hand]
  	(let [fv (freq-vals rank hand)]
  		(= [1 2 2] (sort fv))))

;;sort the ranks
;;create a range that starts at the same first val
;;compare
;;if not equal replace 14 with 1 and try again
(defn straight? [hand]
  (let [r (sort (map rank hand))
		fs (first r)
		rng (range fs (+ fs 5))]
		(= rng r))) 

(defn straight-flush? [hand]
  nil)

(defn value [hand]
  nil)
