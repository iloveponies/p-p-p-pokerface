(ns p-p-p-pokerface)

(defn rank [card]
  (let [
        [fst _] card
        vals { \T 10, \J 11, \Q 12, \K 13, \A 14 }
        ]
    (if (Character/isDigit fst)
      (Integer/valueOf (str fst))
      (vals fst))))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn pair? [hand]
  (let [ranks (map rank hand)
        freqs (set (vals (frequencies ranks)))]
        (and (= (count freqs) 2)
             (= (apply max freqs) 2))))

(defn three-of-a-kind? [hand]
  (let [ranks (map rank hand)
        freqs (set (vals (frequencies ranks)))]
    (= (apply max freqs) 3)))

(defn four-of-a-kind? [hand]
  (let [ranks (map rank hand)
        freqs (set (vals (frequencies ranks)))]
    (= (apply max freqs) 4)))

(defn flush? [hand]
  (let [suits (map suit hand)
        how-many-suits (count (set suits))]
    (= how-many-suits 1)))
    
(defn full-house? [hand]
  (let [ranks (map rank hand)
        freqs (set (vals (frequencies ranks)))]
  (and (contains? freqs 2)
       (contains? freqs 3))))


(defn two-pairs? [hand]
  (let [ranks (map rank hand)
        sorted-freqs (sort (vals (frequencies ranks)))]
    (= sorted-freqs [1 2 2])))

(defn straight? [hand]
  (let [ranks (map rank hand)
        sorted-ranks (sort ranks)
        min-rank (apply min sorted-ranks)
        ranks-compare (range min-rank (+ min-rank 5))]
    (or 
      (= sorted-ranks ranks-compare)
      (and (.contains ranks 14) ; Ace in hand
           (= sorted-ranks '(2 3 4 5 14))))))

(defn straight-flush? [hand]
  (and (flush? hand)
       (straight? hand)))

(defn value [hand]
  (cond (straight-flush? hand)  8
        (four-of-a-kind? hand)  7
        (full-house? hand)      6
        (flush? hand)           5
        (straight? hand)        4
        (three-of-a-kind? hand) 3
        (two-pairs? hand)       2
        (pair? hand)            1
        :else                   0))

