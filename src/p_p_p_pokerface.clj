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
  nil)

(defn two-pairs? [hand]
  ( let [ freqs (vals (frequencies(map rank hand))) ]
    (= 2 (count (filter (fn [x] (= 2 x)) freqs)))))

(defn straight? [hand]
  nil)

(defn straight-flush? [hand]
  nil)

(defn value [hand]
  nil)
