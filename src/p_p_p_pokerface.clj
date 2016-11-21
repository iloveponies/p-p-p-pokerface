(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rank suit] card
        replacements {\T 10 \J 11 \Q 12 \K 13 \A 14}]
    (if (Character/isDigit rank)
      (Integer/valueOf (str rank))
      (get replacements rank))))
      

(defn suit [card]
  (let [[rank suit] card]
    (str suit)))

(defn kind? [num freqs]
  (if (contains? (set (vals freqs)) num)
    true
    false ))

(defn pair? [hand]
  (let [ ranks (map rank hand) 
         freqs (frequencies ranks) ]
    (kind? 2 freqs)))

(defn three-of-a-kind? [hand]
   (let [ ranks (map rank hand) 
         freqs (frequencies ranks) ]
    (kind? 3 freqs))) 

(defn four-of-a-kind? [hand]
   (let [ ranks (map rank hand) 
         freqs (frequencies ranks) ]
    (kind? 4 freqs))) 

(defn flush? [hand]
   (let [ suits (map suit hand) 
         freqs (frequencies suits) ]
    (kind? 5 freqs))) 

(defn full-house? [hand]
  ( let [ card-freqs (set (vals (frequencies(map rank hand)))) ]
    (if (and (contains? card-freqs 3) (contains? card-freqs 2))
      true
      false )))

(defn two-pairs? [hand]
  ( let [ freqs (vals (frequencies(map rank hand))) ]
    (= 2 (count (filter (fn [x] (= 2 x)) freqs)))))

(defn straight? [hand]
  (let [ card-values (sort (map rank hand))
         min-card (apply min card-values)
         straight (range min-card (+ min-card 5)) ]
        (= straight card-values)))

(defn straight-flush? [hand]
  (if (and (straight? hand) (flush? hand))
    true
    false))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                   [two-pairs? 2]  [three-of-a-kind? 3]
                   [straight? 4]   [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}]
    (let [matches (map second (filter (fn [pair] ( let [ checker (first pair)] (checker hand))) checkers))]
         (apply max matches))))
         

