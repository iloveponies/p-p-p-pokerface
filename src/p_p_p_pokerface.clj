(ns p-p-p-pokerface)

(defn rank [card]
  (let [rank-char (get card 0)
        replacements {\T 10
                      \J 11
                      \Q 12
                      \K 13
                      \A 14}]
    (if (Character/isDigit rank-char)
      (Integer/valueOf (str rank-char))
      (get replacements rank-char)))
  )

(defn suit [card]
  (str (get card 1))
  )

(defn pair? [hand]
  (let [ranks (frequencies (map rank hand))
        max-equal (apply max (vals ranks))]
    (>= max-equal 2))
  )

(defn three-of-a-kind? [hand]
  (let [ranks (frequencies (map rank hand))
        max-equal (apply max (vals ranks))]
    (>= max-equal 3))
  )

(defn four-of-a-kind? [hand]
  (let [ranks (frequencies (map rank hand))
        max-equal (apply max (vals ranks))]
    (>= max-equal 4))
  )

(defn flush? [hand]
  (let [suits (frequencies (map suit hand))
        max-equal (apply max (vals suits))]
    (>= max-equal 5))
  )

(defn full-house? [hand]
  (let [ranks (frequencies (map rank hand))
        freqs (sort (vals ranks))
        full-house-freqs [2 3]]
    (= freqs full-house-freqs))
  )

(defn two-pairs? [hand]
  (let [ranks (frequencies (map rank hand))
        freqs (sort (vals ranks))
        two-pairs-freqs #{[1 2 2] [1 4] [5]}]
    (contains? two-pairs-freqs freqs))
  )

(defn straight? [hand]
  (let [ranks (sort (map rank hand))
        low (first ranks)
        high (last ranks)]
    (or (= (range low (inc high )) ranks)
        (= (range 1 6) (sort (replace {14 1} ranks)))))
  )

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand))
  )

(defn value [hand]
  (cond
    (straight-flush? hand) 8
    (four-of-a-kind? hand) 7
    (full-house? hand) 6
    (flush? hand) 5
    (straight? hand) 4
    (three-of-a-kind? hand) 3
    (two-pairs? hand) 2
    (pair? hand) 1
    :else 0)
  )

(defn high-card? [hand]
  true) ; All hands have a high card.

(defn hand [a-hand]
  a-hand)

(defn card [a-card]
  a-card)
