(ns p-p-p-pokerface)




(defn rank [card] (let[[rnk _] card
                       replace {\A 14, \K 13, \Q 12, \J 11, \T 10 }]
                    (if(Character/isDigit rnk) (Integer/valueOf (str rnk)) (replace rnk))))

(defn checkstraight? [hand] (let[rankofhand(sort(map rank hand))
                                lowrank(apply min rankofhand)
                                highrank(apply max rankofhand)
                                rankwitha1(sort(replace { 14 1} rankofhand))]
                             (or (=[1 2 3 4 5] rankwitha1)
                                 (=(range lowrank (+ highrank 1)) rankofhand))))

(defn suit [card] (let [[_ su] card] (str su)))

(defn pair? [hand] (==(count(keys (frequencies(map rank hand)))) 4))

(defn three-of-a-kind? [hand] (==(apply max (vals (frequencies(map rank hand)))) 3))

(defn four-of-a-kind? [hand] (==(apply max (vals (frequencies(map rank hand)))) 4))

(defn flush? [hand] (== (count (frequencies(map suit hand))) 1))

(defn full-house? [hand] ( let [valueofhands(vals (frequencies(map rank hand)))]
                               (and (== (apply max valueofhands) 3)
                                    (==(apply min (rest valueofhands)) 2))))

(defn two-pairs? [hand] ( let [valueofhands(vals (frequencies(map rank hand)))]
                              (and (==  (first valueofhands) 2)
                                   (==(apply max (rest valueofhands)) 2))))

(defn straight? [hand]  (and (>=(count (frequencies(map suit hand))) 2) (checkstraight? hand)))

 (defn straight-flush? [hand] (and (checkstraight? hand) (flush? hand)))

 (defn high-card? [hand] true )
 ;(defn value [hand] (cond
  ;                   (straight-flush? hand) 8
   ;                  (four-of-a-kind? hand) 7
    ;                 (full-house? hand) 6
     ;                (flush? hand) 5
      ;               (straight? hand) 4
       ;              (three-of-a-kind? hand) 3
        ;             (two-pairs? hand) 2
         ;            (pair? hand) 1
          ;           (high-card? hand) 0
           ;          :else 0
            ;         ))

(defn value [hand] (let [checkers #{[high-card? 0] [pair? 1]
                                    [two-pairs? 2]  [three-of-a-kind? 3]
                                    [straight? 4]   [flush? 5]
                                    [full-house? 6] [four-of-a-kind? 7]
                                    [straight-flush? 8]}
          checkhand ( fn [[check value]] (if (check hand) value 0))]
                     (apply max (map checkhand checkers))))






