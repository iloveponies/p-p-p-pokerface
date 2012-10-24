(ns p-p-p-pokerface)

(defn rank [card]
   (let [[rank _] card]
    (if(Character/isDigit rank)
     (Integer/valueOf (str rank))
       (get {\T 10, \J 11, \Q 12, \K 13, \A 14} rank))))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn pair? [hand]
  (if ( == (apply max(vals(frequencies(map rank hand)))) 2 )
    true
    false))

(defn three-of-a-kind? [hand]
   (if ( == (apply max(vals(frequencies(map rank hand)))) 3 )
    true
    false))

(defn four-of-a-kind? [hand]
    (if ( == (apply max(vals(frequencies(map rank hand)))) 4 )
    true
    false))

(defn flush? [hand]
    (if (apply = (map suit hand))
    true
    false))

(defn full-house? [hand]
  (let [v (vals (frequencies(map rank hand)))]
    (if (= (sort v) (seq [2 3]))
      true
      false
      )))

(defn two-pairs? [hand]
   (let [v (vals (frequencies(map rank hand)))]
    (if (or (= (sort v) (seq [1 4])) (= (sort v) (seq [1 2 2])))
      true
      false
      )))

(defn straight? [hand]
  (let [s1 (sort (map rank hand))
        sm1 (first s1)
        s2 (sort (replace {14 1} s1))
        sm2 (first s2)
        ]
    (if (or (= s1 (range sm1 (+ sm1 5))) (= s2 (range sm2 (+ sm2 5))))
      true
      false
      )))

(defn straight-flush? [hand]
  (if (and (flush? hand) (straight? hand))
    true
    false
    ))

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
   :else 0
   ))
