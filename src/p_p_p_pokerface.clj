(ns p-p-p-pokerface)

(def values {\T 10 \J 11 \Q 12 \K 13 \A 14})

(defn rank [card]
  (let [ [r _] card ]
    (if (Character/isDigit r)
      (Integer/valueOf (str r))
      (Integer/valueOf (str (values r)))
  )))

(defn suit [card]
    (let [ [_ s]    card ] (str s))
)


(defn rank-freq [hand]
  (frequencies (map rank hand))
)

(defn max-freq [hand]
  (apply max (vals (rank-freq hand)))
)

(defn pair? [hand]
  (= (max-freq hand) 2)
)

(defn three-of-a-kind? [hand]
  (= (max-freq hand) 3)
)

(defn four-of-a-kind? [hand]
  (= (max-freq hand) 4)
)

(defn flush? [hand]
  (= (count (set (map suit hand))) 1)
)

(defn full-house? [hand]
  (= [2 3] (sort (vals (rank-freq hand))))
)

(defn two-pairs? [hand]
     (and
        (pair? hand)
        (= 3 (count (rank-freq hand)))
     )
)

(defn straight? [hand]
  (let  [ sorted-ranks (sort (map rank hand))
          first-rank (first sorted-ranks)
          normalize  (fn [r] (- r first-rank))
          normalized-rank (map normalize sorted-ranks)
        ]
  (or
    (= normalized-rank [0 1 2 3 4] )
    (= normalized-rank [0 1 2 3 12] )
  )))

(defn straight-flush? [hand]
  (and (flush? hand) (straight? hand))
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
