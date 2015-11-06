(ns p-p-p-pokerface)

(defn rank [card]
  (let [[r _] [(first card) (str(second card))]]
    (def replacements {\T 10, \J 11, \Q 12, \K 13, \A 14})
      (if(Character/isDigit r)
        (Integer/valueOf (str r)) ;1-9 converted to integer
        (replacements r)))) ;character to number conversion

(defn suit [card]
  (let [[_ s] [(str(first card)) (str(second card))]]
    (str s)))

(defn pair? [hand]
  (let [ranks (map rank hand)
        pairs (apply max(vals(frequencies ranks)))]
          (< 1 pairs)))

(defn three-of-a-kind? [hand]
  (let [ranks (map rank hand)
        pairs (apply max(vals(frequencies ranks)))]
          (< 2 pairs)))

(defn four-of-a-kind? [hand]
  (let [ranks (map rank hand)
        pairs (apply max(vals(frequencies ranks)))]
          (< 3 pairs)))

(defn flush? [hand]
  (let [suits (map suit hand)
        pairs (apply max(vals(frequencies suits)))]
          (= 5 pairs)))
(defn full-house? [hand]
  (let [groups (sort(vals(frequencies (map rank hand))))]
    (and (= 2 (first groups)) 
         (= 3 (second groups)))))

(defn two-pairs? [hand]
  (let [groups (sort(vals(frequencies (map rank hand))))]
    (and (= 2 (second groups))
         (= 2 (nth groups 2)))))
(defn straight? [hand]
  (let [ranks-a (sort (map rank hand)) ;aces as fourteens
        ranks-b (sort (replace {14 1} ranks-a)) ;aces as ones
        comp-a (range (first ranks-a) (+ 5 (first ranks-a))) ;these are what the straights
        comp-b (range (first ranks-b) (+ 5 (first ranks-b))) ;should look like
        ]
    (cond
      (= ranks-a comp-a) true
      (= ranks-b comp-b) true
      :else false)))

(defn straight-flush? [hand]
    (and (flush? hand) (straight? hand)))

(defn value [hand] ;trololo, didn't learn a thing
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
