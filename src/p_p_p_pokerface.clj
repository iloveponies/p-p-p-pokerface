(ns p-p-p-pokerface)

(defn rank [card]
  (let [[ rank _] card
        rank-map {\2 2
                  \3 3
                  \4 4
                  \5 5
                  \6 6
                  \7 7
                  \8 8
                  \9 9
                  \T 10
                  \J 11
                  \Q 12
                  \K 13
                  \A 14}]
  (get rank-map rank))
)

(defn suit [card]
  (let [[ _ suit] card]
    (str suit)
  )
)

(defn pair? [hand]
  (let [ranks (map rank hand)
        freqs (frequencies ranks)
        values (vals freqs)
         ]
    (> (apply max values) 1)
  )
)

(defn three-of-a-kind? [hand]
  (let [ranks (map rank hand)
        freqs (frequencies ranks)
        values (vals freqs)
         ]
    (> (apply max values) 2)
  )
)

(defn four-of-a-kind? [hand]
  (let [ranks (map rank hand)
        freqs (frequencies ranks)
        values (vals freqs)
         ]
    (> (apply max values) 3)
  )
)

(defn flush? [hand]
  (let [suits (map suit hand)
        freqs (frequencies suits)]
    (= (count freqs) 1)
  )
)

(defn full-house? [hand]
  (let [ranks (map rank hand)
        freqs (frequencies ranks)
        values (vals freqs)
         ]
    (and (= (apply max values) 3)
      (= (apply min values) 2))
  )
)

(defn two-pairs? [hand]
  (let [ranks (map rank hand)
        freqs (frequencies ranks)
        values (vals freqs)
        val-freqs (frequencies values)
        val-vals (vals val-freqs)
        sorted (sort val-vals)]
          (and (< (apply max values) 3)
               (= 2(count val-freqs))
               (= 1 (first sorted))
               (= 2 (last sorted)))
  )
)

(defn straight? [hand]
  (let [
    ranks (map rank hand)

    replace-if-needed (fn [ranks] (let [
      max-rank (apply max ranks)
      min-rank (apply min ranks)
      ]
      (if (and (= max-rank 14) (= min-rank 2))
           (replace {14 1} ranks)
           ranks)
      )
    )

    check-straight (fn [ranks] (let [
      max-rank (apply max ranks)
      min-rank (apply min ranks)
      freqs (frequencies ranks)
      values (vals freqs)
      val-freqs (frequencies values)
      val-vals (vals val-freqs)
      ]
        (and (= 4 (- max-rank min-rank))
             (= (first val-vals) 5))
      ))

    ]

    (check-straight (replace-if-needed ranks))
  )
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
    :else 0
    )
)
