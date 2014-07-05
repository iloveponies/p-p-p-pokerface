(ns p-p-p-pokerface)

(def rank-map {\T 10 \J 11 \Q 12 \K 13 \A 14})

(defn rank [[card-rank card-suit]]
  (cond
    (Character/isDigit card-rank) (Integer/valueOf (str card-rank))
    :else (get rank-map card-rank)))
    

(defn suit [[card-rank card-suit]]
  (str card-suit))

(defn has-combo-of-size? [hand size]
  (let [ranks (map rank hand)
        rank-freqs-map (frequencies ranks)    ; ranks->frequency count
        rank-freqs (vals rank-freqs-map)      ; just frequencies of ranks
        combos-freqs (frequencies rank-freqs) ; frequencies of rank combinations
        ]
    ; See if there is a combo of exactly size
    (contains? (set (keys combos-freqs)) size)))

(defn pair? [hand]
  (has-combo-of-size? hand 2))

(defn three-of-a-kind? [hand]
  (has-combo-of-size? hand 3))

(defn four-of-a-kind? [hand]
  (has-combo-of-size? hand 4))

(defn flush? [hand]
  nil)

(defn full-house? [hand]
  (and (pair? hand) (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  nil)

(defn straight? [hand]
  nil)

(defn straight-flush? [hand]
  nil)

(defn value [hand]
  nil)
