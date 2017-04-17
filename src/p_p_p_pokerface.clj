(ns p-p-p-pokerface)

(defn rank [card]
	(let [[fst _] card]
	 (if (Character/isDigit fst) 
	    (Integer/valueOf (str fst))
	    (get {\A 14, \K 13, \Q 12, \J 11, \T 10} fst)))
)

(defn suit [card]
	(let [[_ snd] card]
 	 (str snd))
)

(defn pair? [hand]
 nil)

(defn three-of-a-kind? [hand]
  nil)

(defn four-of-a-kind? [hand]
  nil)

(defn flush? [hand]
  nil)

(defn full-house? [hand]
  nil)

(defn two-pairs? [hand]
  nil)

(defn straight? [hand]
  nil)

(defn straight-flush? [hand]
  nil)

(defn value [hand]
  nil)
