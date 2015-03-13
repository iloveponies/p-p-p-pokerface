(ns p-p-p-pokerface)

(defn rank [card]
  (let [[r _] card]
    (if (Character/isDigit r)
      (Integer/valueOf (str r))
      (get 
        { \T 10, \J 11, \Q 12, \K 13, \A 14} 
        r )
      )))

(defn suit [card]
  (let [[_ s] card]
      (str s)))

(defn pair? [hand]
  (=
    (apply max (vals (frequencies (map rank hand))))
    2))

(defn three-of-a-kind? [hand]
  (=
    (apply max (vals (frequencies (map rank hand))))
    3))

(defn four-of-a-kind? [hand]
  (=
    (apply max (vals (frequencies (map rank hand))))
    4))

(defn flush? [hand]
  (=
   (apply max (vals (frequencies (map suit hand))))
   5)
  )

(defn full-house? [hand]
  (let [freqs (frequencies (map rank hand))]
    (and
      (= 3 (apply max (vals freqs)))
      (= 2 (apply min (vals freqs))))
    ))

(defn two-pairs? [hand]
  (let [freqs (vals (frequencies (map rank hand)))]
    (and
      (= 3 (count freqs))
      (= 2 (apply max freqs))
      )
    ))

(defn straight? [hand]
  (let [testRange (fn [r] (= 4 (- 
                         (apply max r)
                         (apply min r))))
        ranks (map rank hand)
        altRanks (replace {14 1} ranks)]
    (and (not (pair? hand))
         (or
           (testRange ranks)
           (testRange altRanks))
         )))

(defn straight-flush? [hand]
  (and
    (straight? hand)
    (flush? hand)))

(defn high-card? [hand] true)

(defn value [hand]
  (let [checkers #{
                   [high-card? 0]  
                   [pair? 1]
                   [two-pairs? 2]  
                   [three-of-a-kind? 3]
                   [straight? 4]  
                   [flush? 5]
                   [full-house? 6]
                   [four-of-a-kind? 7]
                   [straight-flush? 8]
                   }]
    (apply max (map second (filter 
                 (fn [[key val]] (key hand))
                 checkers))
           )))

