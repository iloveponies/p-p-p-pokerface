(ns p-p-p-pokerface)

(def rank-map {\T 10 \J 11 \Q 12 \K 13 \A 14})

(defn rank [[card-rank card-suit]]
  (cond
    (Character/isDigit card-rank) (Integer/valueOf (str card-rank))
    :else (get rank-map card-rank)))
    

(defn suit [[card-rank card-suit]]
  (str card-suit))

;(defn has-combo-of-size? [hand size]
;  )

(defn pair? [hand]
  (let [ranks (map rank hand)
        rank-freqs-map (frequencies ranks)    ; ranks->frequency count
        rank-freqs (vals rank-freqs-map)      ; just frequencies of ranks
        combos-freqs (frequencies rank-freqs) ; frequencies of rank combinations
        ]
    ; A pair is a combination of exactly 2 cards with the same rank
    (contains? (set (keys combos-freqs)) 2)))
        
    

(defn three-of-a-kind? [hand]
  nil)

(defn four-of-a-kind? [hand]
  nil)

(defn flush? [hand]
  nil)

(defn full-house? [hand]
  nil)

(defn two-pairs? [hand]
  nil)

(defn straight? [hand]
  nil)

(defn straight-flush? [hand]
  nil)

(defn value [hand]
  nil)
