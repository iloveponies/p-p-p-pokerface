(ns p-p-p-pokerface)

(defn rank [card]
  (let [[snd _] card] snd
    (def replacements {\A 14, \K 13, \Q 12, \J 11, \T 10})
    (if (Character/isDigit snd)
       (Integer/valueOf (str snd))
        (replacements snd))))


; (Character/isDigit \5) ;=> true
; (Integer/valueOf "12") ;=> 12
; (Integer/valueOf (str \5)) ;=> 5

(defn suit [card]
  (str (let [[_ snd] card]
    snd)))

; (let [[_ snd] "AH"]
;    snd) ;=> \H
;
;    (str \C) ;=> "C"


(defn pair? [hand]
  (= (apply max (vals (frequencies (map rank hand)))) 2))

(defn three-of-a-kind? [hand]
  (= (apply max (vals (frequencies (map rank hand)))) 3))

(defn four-of-a-kind? [hand]
  (= (apply max (vals (frequencies (map rank hand)))) 4))

(defn flush? [hand]
  (= (apply max (vals (frequencies (map suit hand)))) 5))

(defn full-house? [hand]
  (and (= (apply max (vals (frequencies (map rank hand)))) 3)
       (= (apply min (vals (frequencies (map rank hand)))) 2)))

(defn two-pairs? [hand]
  (if (or (and (pair? hand) (= (second (vals (frequencies (map rank hand)))) 2)) (four-of-a-kind? hand))
    true
    false))

(defn straight? [hand]
 (let [sorted (sort (map rank hand)) smallest (first sorted)
       alt (sort (replace {14 1} sorted)) alt-smallest (first alt)]
   (or 
     (= sorted (range smallest (+ smallest 5)))
     ( = alt (range alt-smallest (+ alt-smallest 5)))))) 

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

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
