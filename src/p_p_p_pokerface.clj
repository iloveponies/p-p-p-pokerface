(ns p-p-p-pokerface)

; I decided to hard-code all the card-ranks. In a program like this I see little reason
; to hard-code only a part of the card-rank-table. Plus it simplifies the function quite a bit.

(defn rank [[fst _]]
  "Returns the rank of a card."
  (let [vals {\2 2 \3 3 \4 4 \5 5 \6 6 \7 7 \8 8 \9 9 \T 10 \J 11 \Q 12 \K 13 \A 14}]
	(get vals fst)) )

(defn suit [[_ snd]]
  "Returns the suit of a card."
  (str snd) )

(defn member [val col]
  "checks whether val is an element of the collection col.
   Returns the entire tail of the collection beginning with
   the element val if found, false otherwise (just like
   in Common Lisp)"
  (cond
    (empty? col) false
	(= (first col) val) col
	:else (member val (rest col)) ) )
	
(defn has-n-same-types? [n type-f hand]
  "Returns true if hand contains n cards
   of a same type-f (rank or suit),
   false otherwise"
  (let [hand-freqs (vals (frequencies (map type-f hand)))]
    (boolean (member n hand-freqs)) ) )
	
(defn pair? [hand]
  "Returns true if hand contains a pair,
   false otherwise."
 (has-n-same-types? 2 rank hand) )

(defn three-of-a-kind? [hand]
  "Returns true if hand contains a three-of-a-kind,
   false otherwise."
 (has-n-same-types? 3 rank hand) )

(defn four-of-a-kind? [hand]
  "Returns true if hand contains a four-of-a-kind,
   false otherwise."
 (has-n-same-types? 4 rank hand) )

(defn flush? [hand]
  "Returns true if hand contains a flush,
   false otherwise."
  (has-n-same-types? 5 suit hand) )

(defn full-house? [hand]
  "Returns true if hand contains a full house,
   false otherwise."
  (and (pair? hand) (three-of-a-kind? hand)) )

  
  
 ; The following function might be one hack too much...
 ; it heavily relies on the behaviour of the Common Lisp-style
 ; member function.
 
(defn two-pairs? [hand]
  (if (four-of-a-kind? hand)
	  true
     (let [hand-freqs (vals (frequencies (map rank hand)))
	       first-pair-found (member 2 hand-freqs) ]
        (boolean
	      (and first-pair-found (member 2 (rest first-pair-found))) ) ) ) )
	
; the function above, using what was suggested in the course material:
;
; (defn two-pairs? [hand]
;   (let [hand-freqs (sort (vals (frequencies (map rank hand))))]
;     (or? (= (rest hand-freqs) '(2 2))
;		   (four-of-a-kind? hand) ) ) )
;
; or something like that...



(def low-straight '(2 3 4 5 14))

(defn straight? [hand]
  (let [sorted-ranks (sort (map rank hand))
		min-rank (first sorted-ranks)]
    (or (= sorted-ranks (range min-rank (+ min-rank 5)))
		(= sorted-ranks low-straight) ) ) )

(defn straight-flush? [hand]
  (and (flush? hand) (straight? hand)) )
  
(defn value [hand]
  (let [checkers #{[pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}]
	(apply max
	      (map (fn [checker]
	             (if ((first checker) hand) (second checker) 0))
		        checkers )) ) )