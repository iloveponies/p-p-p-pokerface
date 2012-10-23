(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rank suit] card]
    (cond 
     (Character/isDigit rank) (Integer/valueOf (str rank))
     (= \T rank) 10
     (= \J rank) 11
     (= \Q rank) 12
     (= \K rank) 13
     (= \A rank) 14)))

(defn suit [card]
  (let [[rank suit] card]
    (str suit)))

(defn pair? [hand]
  (let [ranks (map rank hand)]
    (< 1 (apply max (vals (frequencies ranks))))))

(defn three-of-a-kind? [hand]
  (let [ranks (map rank hand)]
    (< 2 (apply max (vals (frequencies ranks))))))

(defn four-of-a-kind? [hand]
  (let [ranks (map rank hand)]
    (< 3 (apply max (vals (frequencies ranks))))))

(defn flush? [hand] 
  (let [suits (map suit hand)]
    (== 5 (apply max (vals (frequencies suits))))))

(defn full-house? [hand] 
  (let [ranks (map rank hand)
        freq-vals (vals (frequencies ranks))]
    (and (== 2 (apply min freq-vals)) 
         (== 3 (apply max freq-vals)))))

(defn two-pairs? [hand] 
  (let [ranks (map rank hand)
        freq-vals (vals (frequencies ranks))]
    (or 
     (and (== 3 (count freq-vals)) 	 ; 3,1,1 or 2,2,1 
          (== 2 (apply max freq-vals)))
     (== 4 (apply max freq-vals))))) ; 4-of-kind

(defn straight? [hand] 
  (let [sorted-ranks (apply vector (sort (map rank hand))) ; :<
        swapped-ace-ranks 
        (if (contains? (set sorted-ranks) 14) ; horror, HORROR
          (if (== 2 (get sorted-ranks 0))
          	(sort (assoc sorted-ranks 4 1)) ; ace is always last
          	sorted-ranks)
          sorted-ranks)
        straight-from? (fn [x] (= (range x (+ x 5)) 
                                  swapped-ace-ranks))]
    (if (empty? (filter straight-from? (range 1 11))) 
      false 
      true)))

(defn straight-flush? [hand] 
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand] true)

(defn hand-has-value? [hand value] 
  (let [checkers [high-card?, pair?, two-pairs?, 
                  three-of-a-kind?, straight?, flush?,
                  full-house?, four-of-a-kind?, 
                  straight-flush?]]
    ((get checkers value) hand)))

(defn value [hand] 
  (let [has-value? (fn [x] (hand-has-value? hand x))]
    (apply max (filter has-value? (range 0 9)))))