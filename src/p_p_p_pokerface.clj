(ns p-p-p-pokerface)

(def rank-map {\T 10 \J 11 \Q 12 \K 13 \A 14})

(defn rank [[card-rank card-suit]]
  (cond
    (Character/isDigit card-rank) (Integer/valueOf (str card-rank))
    :else (get rank-map card-rank)))
    

(defn suit [[card-rank card-suit]]
  (str card-suit))

; Get map of the frequencies of different combinations
; (how many pairs, how many three-of-a-kinds, etc.)
(defn combo-frequencies [hand]
  (let [ranks (map rank hand)
        rank-freqs-map (frequencies ranks)    ; ranks->frequency count
        rank-freqs (vals rank-freqs-map)      ; just frequencies of ranks
        combos-freqs (frequencies rank-freqs) ; frequencies of rank combinations
        ]
    combos-freqs))

(defn has-combo-of-size? [hand size]
  (let [combos-freqs (combo-frequencies hand)]
    ; See if there is a combo of exactly size
    (contains? (set (keys combos-freqs)) size)))

(defn pair? [hand]
  (has-combo-of-size? hand 2))

(defn three-of-a-kind? [hand]
  (has-combo-of-size? hand 3))

(defn four-of-a-kind? [hand]
  (has-combo-of-size? hand 4))

(defn flush? [hand]
  (let [suit-frequencies (frequencies (map suit hand))]
    (= (count suit-frequencies) 1)))

(defn full-house? [hand]
  (and (pair? hand) (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  (let [combos-freqs (combo-frequencies hand)]
    (= (get combos-freqs 2) 2)))

; Test vector of ranks to see if it is a straight. 
(defn straight-ranks? [ranks]
  (let [ranks-sorted (sort ranks)
        start (first ranks-sorted)
        size (count ranks)]
    (= ranks-sorted (range start (+ start size)))))

(defn straight? [hand]
  (let [ranks (map rank hand)
        ranks-ace-low (replace {14 1} ranks)]
    (or (straight-ranks? ranks) (straight-ranks? ranks-ace-low))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn value [hand]
  nil)
