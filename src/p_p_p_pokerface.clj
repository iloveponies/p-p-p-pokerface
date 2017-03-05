(ns p-p-p-pokerface)

(defn rank [[card-rank]]
  (let [replacements {\T 10 \J 11 \Q 12 \K 13 \A 14}]
    (if (Character/isDigit card-rank)
        (Integer/valueOf (str card-rank))
        (replacements card-rank))))

(defn suit [[_ card-suit]]
  (str card-suit))


(defn has-cards? [hand cards]
  (let [card-configs
          {:pair           [rank 1 1 1 2]
           :three-of-kind  [rank 1 1 3]
           :four-of-kind   [rank 1 4]
           :flush          [suit 5]
           :full-house     [rank 2 3]
           :two-pairs      [rank 1 2 2]}
        config
          (card-configs cards)
        card-type
          (first config)
        all-types 
          (map card-type hand)
        sorted-kind-freq 
          (sort (vals (frequencies all-types)))]
    
  (= sorted-kind-freq (rest config))))


(defn has-consecutive-ranks? [hand replacement-map]
  (let [sorted-ranks
          (sort (map rank (replace replacement-map hand)))
        min-rank
          (apply min sorted-ranks)
        normalize-ranks
          (fn [rank-value] (- rank-value min-rank))]
    
  (= (map normalize-ranks sorted-ranks) [0 1 2 3 4])))



(defn pair? [hand]
  (has-cards? hand :pair))

(defn three-of-a-kind? [hand]
  (has-cards? hand :three-of-kind))

(defn four-of-a-kind? [hand]
  (has-cards? hand :four-of-kind))

(defn flush? [hand]
  (has-cards? hand :flush))

(defn full-house? [hand]
  (has-cards? hand :full-house))

(defn two-pairs? [hand]
  (has-cards? hand :two-pairs))

(defn straight? [hand]
  (let [replacement-map {"AC" "1C", "AS" "1S", "AD" "1D", "AH" "1H"}]
  (or (has-consecutive-ranks? hand {}) 
      (has-consecutive-ranks? hand replacement-map))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

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
    :else                     0))
