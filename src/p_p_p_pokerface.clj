(ns p-p-p-pokerface)

(defn rank [[cardRank _]]  (let [xr ({\T 10 \J 11 \Q 12 \K 13 \A 14} cardRank)]
                             (if (nil? xr) (Integer/valueOf (str cardRank)) xr)))  
;(rank "JS") ;=> 11

(defn suit [[_ cardSuit]] (str cardSuit) 
  )
;(suit "2H") ;=> "H"


(defn pair? [hand] (not (nil? (some #(> (last %) 1) (frequencies (map rank hand)))))
  )
;(pair? pair-hand)  ;=> true
;(pair? high-seven) ;=> false

(defn three-of-a-kind? [hand] (not (nil? (some #(> (last %) 2) (frequencies (map rank hand)))))
  )
;(three-of-a-kind? two-pairs-hand)       ;=> false
;(three-of-a-kind? three-of-a-kind-hand) ;=> true

(defn four-of-a-kind? [hand] (not (nil? (some #(> (last %) 3) (frequencies (map rank hand)))))
  )
;(four-of-a-kind? two-pairs-hand)      ;=> false
;(four-of-a-kind? four-of-a-kind-hand) ;=> true

(defn flush? [hand](= 5 (last (first (frequencies (map suit hand)))))
 )
;(flush? pair-hand)  ;=> false
;(flush? flush-hand) ;=> true

(defn full-house? [hand] (= #{2, 3} (set (vals (frequencies (map rank hand)))))
  )
;(full-house? three-of-a-kind-hand) ;=> false
;(full-house? full-house-hand)      ;=> true


(defn two-pairs? [hand] (let [c (filter #(> % 1) (vals (frequencies (map rank hand))))]
                          (if (or (> (count c) 1) (four-of-a-kind? hand)) true false)) 
  )
;(two-pairs? two-pairs-hand)      ;=> true
;(two-pairs? pair-hand)           ;=> false
;(two-pairs? four-of-a-kind-hand) ;=> true

(defn straight? [hand] (let [s1 (sort (map rank hand)) s2 (sort (replace {14 1} s1)) ]
                             (or (= s1 (range (first s1) (+ 5 (first s1))))
                                 (= s2 (range (first s2) (+ 5 (first s2))))))

  )
;(straight? two-pairs-hand)             ;=> false
;(straight? straight-hand)              ;=> true
;(straight? low-ace-straight-hand)      ;=> true
;(straight? ["2H" "2D" "3H" "4H" "5H"]) ;=> false
;(straight? high-ace-straight-hand)     ;=> true

(defn straight-flush? [hand] (and (straight? hand) (flush? hand))
  )
;(straight-flush? straight-hand)                ;=> false
;(straight-flush? flush-hand)                   ;=> false
;(straight-flush? straight-flush-hand)          ;=> true
;(straight-flush? low-ace-straight-flush-hand)  ;=> true
;(straight-flush? high-ace-straight-flush-hand) ;=> true


(defn value [hand] (cond
                     (straight-flush? hand) 8
                     (four-of-a-kind? hand) 7
                     (full-house? hand) 6
                     (flush? hand) 5
                     (straight? hand) 4
                     (three-of-a-kind? hand) 3
                     (two-pairs? hand) 2
                     (pair? hand) 1
                     :else 0)
  )
;(value high-seven)           ;=> 0
;(value pair-hand)            ;=> 1
;(value two-pairs-hand)       ;=> 2
;(value three-of-a-kind-hand) ;=> 3
;(value straight-hand)        ;=> 4
;(value flush-hand)           ;=> 5
;(value full-house-hand)      ;=> 6
;(value four-of-a-kind-hand)  ;=> 7
;(value straight-flush-hand)  ;=> 8


