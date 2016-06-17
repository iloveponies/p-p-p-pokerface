(ns p-p-p-pokerface)

(defn rank [card]
  (def replacements {\T 10, \J 11, \Q 12, \K 13, \A 14})
  (let [char-rank (first card)]
    (if(java.lang.Character/isDigit char-rank)
     (java.lang.Integer/valueOf (str char-rank))
     (replacements char-rank))))

(defn suit [card]
  (str (get card 1)))

(defn pair? [hand]
  (>= (apply max (vals (frequencies (map first hand)))) 2))

(defn three-of-a-kind? [hand]
  (>= (apply max (vals (frequencies (map first hand)))) 3))

(defn four-of-a-kind? [hand]
  (>= (apply max (vals (frequencies (map first hand)))) 4))

(defn flush? [hand]
  (== (apply max (vals (frequencies (map suit hand)))) 5))

(defn full-house? [hand]
  (= [2 3] (sort (vals (frequencies (map rank hand))))))

(defn two-pairs? [hand]
  (or (four-of-a-kind? hand) (full-house? hand) (= [1 2 2] (sort (vals (frequencies (map rank hand)))))))

(defn straight? [hand]
  (let [hands (map rank hand)
        max-rank (apply max hands)
        min-rank (apply min hands)]
    (if (and (==  max-rank 14) (== min-rank 2))
      (let [sorted-ranks (sort (replace {14 1} hands))
            max-rank-new (apply max sorted-ranks)
            min-rank-new (first sorted-ranks)]
        (= sorted-ranks (range min-rank-new (+ max-rank-new 1))))
      (= (sort hands) (range min-rank (+ max-rank 1))))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}]
    (apply max (map second (filter (fn [x] ((first x) hand)) checkers)))
    
    )
  
  )
