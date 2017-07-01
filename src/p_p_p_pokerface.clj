(ns p-p-p-pokerface)
 
 
(def replacements {\T 10, \J 11, \Q 12, \K 13, \A 14})
 
 
 
(defn rank [card]
  (let[[rank _] card]
    (if (Character/isDigit rank)
      (Integer/valueOf (str rank))
       (replacements rank))))
 
(defn suit [card]
  (let[[_ suit] card]
    (str suit)))
 
(defn pair? [hand]
  (> (apply max (vals (frequencies (map rank hand)))) 1))
 
 
(defn three-of-a-kind? [hand]
  (and
   (== (apply max (vals(frequencies (map rank hand)))) 3)
   (== (apply min (vals(frequencies (map rank hand)))) 1)))
 
 
(defn four-of-a-kind? [hand]
  (== (apply max (vals(frequencies (map rank hand)))) 4))
 
(defn flush? [hand]
  (== (count(frequencies(map suit hand))) 1))
 
(defn full-house? [hand]
  (and
   (==(apply max (vals(frequencies (map rank hand)))) 3)
   (==(apply min (vals(frequencies (map rank hand)))) 2)))
 
(defn two-pairs? [hand]
  (== 2 (apply min (vals (frequencies (rest (sort (vals (frequencies (map rank hand))))))))))
 
(defn straight? [hand]
  (let [ace-high (sort (map rank hand)) ace-low (sort (replace {14 1} (map rank hand)))]
    (if (= ace-high (range (apply min ace-high) (+ (apply min ace-high) 5)))
      true
      (= ace-low (range (apply min ace-low) (+ (apply min ace-low) 5))))))
 
(defn straight-flush? [hand]
  (and
   (straight? hand)
   (flush? hand)))
 
 
(defn value [hand]
  (cond
    (straight-flush? hand)    8
    (four-of-a-kind? hand)    7
    (full-house? hand)        6
    (flush? hand)             5
    (straight? hand)          4
    (three-of-a-kind? hand)   3
    (two-pairs? hand)         2
    (pair? hand)              1
    :else                     0
    ))
 
 
