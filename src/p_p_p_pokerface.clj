(ns p-p-p-pokerface)

(defn rank [card]
 (let [[fst snd] card]
    (if (Character/isDigit fst)
    (Integer/valueOf (str fst))
({\T 10, \J 11, \Q 12, \K 13, \A 14} fst))))

(defn suit [card]
(str (let [[_ snd] card]
snd)))

(defn pair? [hand]
   ( contains? (set (vals (frequencies (map rank hand)) ))2)
)


(defn three-of-a-kind? [hand]
     ( contains? (set (vals (frequencies (map rank hand)) ))3)
)


(defn four-of-a-kind? [hand]
   ( contains? (set (vals (frequencies (map rank hand)) ))4)
)


(defn flush? [hand]
  (= (count (keys (frequencies ( map suit hand)))) 1)

)

(defn full-house? [hand]
(= [2 3] (sort (into [] (seq (vals (frequencies (map rank hand)))))))
)

(defn two-pairs? [hand]
 (= (first (max (vals (frequencies (seq (vals (frequencies (map rank hand)))))))) 2 )
)

(defn straight? [hand]
   (let [handy (set ( map rank hand))]
    (if (contains? handy 2)
     ; (and (contains?  (set (sort (replace {14, 1} handy)  )) 3))
      (and(< (- (apply max (set (sort (replace {14, 1} handy)  )))
           (apply min (set (sort (replace {14, 1} handy)  ))))5)
          (= (count (vals (frequencies (map rank hand)))) 5)
)
 (and(< (- (apply max (set (sort  handy  )))
           (apply min (set (sort  handy  ))))5)
          (= (count (vals (frequencies (map rank hand)))) 5)
)
)))


(defn straight-flush? [hand]
     (let [handy (set ( map rank hand))]
    (if (contains? handy 2)
     ; (and (contains?  (set (sort (replace {14, 1} handy)  )) 3))
      (and(< (- (apply max (set (sort (replace {14, 1} handy)  )))
           (apply min (set (sort (replace {14, 1} handy)  ))))5)
          (= (count (vals (frequencies (map rank hand)))) 5)
(flush? hand)
)
 (and(< (- (apply max (set (sort  handy  )))
           (apply min (set (sort  handy  ))))5)
          (= (count (vals (frequencies (map rank hand)))) 5)
(flush? hand)
)
)))

(defn value [hand]
 (cond
(full-house? hand) 6
(two-pairs? hand) 2
(pair? hand) 1
(three-of-a-kind? hand) 3
(straight-flush? hand) 8
(straight? hand) 4
(flush? hand) 5
(four-of-a-kind? hand) 7

:else 0)
)
