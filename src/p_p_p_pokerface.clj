(ns p-p-p-pokerface)

(defn rank [card]
  (let [ranks {\A 14 \K 13 \Q 12 \J 11 \T 10}]
    (if (Character/isDigit (get card 0))
      (Integer/valueOf (str (get card 0)))
      (ranks (get card 0))
    )
  )
)

(defn suit [card]
  (str (get card 1))
)

(defn pair? [hand]
  (let [ranks (map rank hand)
        values (vals (frequencies ranks))]
    (if (> (count (filter (fn [num] (if (== num 2) true false)) values)) 0)
      true
      false
    )
  )
)

(defn three-of-a-kind? [hand]
  (let [ranks (map rank hand)
        values (vals (frequencies ranks))]
    (if (> (count (filter (fn [num] (if (== num 3) true false)) values)) 0)
      true
      false
    )
  )
)


(defn four-of-a-kind? [hand]
  (let [ranks (map rank hand)
        values (vals (frequencies ranks))]
    (if (> (count (filter (fn [num] (if (== num 4) true false)) values)) 0)
      true
      false
    )
  )
)

(defn flush? [hand]
  (let [suits (frequencies (map suit hand))]
    (if (== (count suits) 1) true false)
  )
)

(defn full-house? [hand]
  (let [values (sort(vals(frequencies(map rank hand))))
        increasing? (fn [a-seq] (if (apply < a-seq) true false))]
    (if (and (increasing? values) (not (four-of-a-kind? hand))) true false)
  )
)

(defn two-pairs? [hand]
  (let [ranks  (map rank hand)
        values (vals (frequencies ranks))
        twos   (frequencies values)]
    (if (or (== (get twos 2 0) 2) (four-of-a-kind? hand)) true false)
  )
)

(defn straight? [hand]
  (let [ranks (sort(map rank hand))
        increasing? (fn [a-seq] (if (apply < a-seq) true false))
        really-straight? (fn [a-seq] (and
                                         (increasing? a-seq)
                                         (== (- (last a-seq) (first a-seq)) 4 )))]
    (if (or(== (last ranks) 14)
        (really-straight? (replace {14 1} ranks))
        (really-straight? ranks)) true false)
  )
)

(defn straight-flush? [hand]
  (if (and (straight? hand) (flush? hand)) true false))

(defn value [hand]
  (cond
    (straight-flush? hand)  8
    (four-of-a-kind? hand)  7
    (full-house? hand)      6
    (flush? hand)           5
    (straight? hand)        4
    (three-of-a-kind? hand) 3
    (two-pairs? hand)       2
    (pair? hand)            1
    :else                   0
  )
)
