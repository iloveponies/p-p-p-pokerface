(ns p-p-p-pokerface)

(defn rank [card]
  (let [[ran _] card
        lol {\T 10,
             \J 11,
             \Q 12,
             \K 13,
             \A 14}
        ]
    (if (Character/isDigit ran)
      (Integer/valueOf (str ran))
      (get lol ran)))
  )

(defn suit [card]
  (let [[_ snd] card]
    (str snd))
  )

(defn pair? [hand]
  (let [freqs (vals (frequencies (map rank hand)))]
    (< 1 (apply max freqs)))
  )

(defn three-of-a-kind? [hand]
  (let [freqs (vals (frequencies (map rank hand)))]
    (< 2 (apply max freqs)))
  )

(defn four-of-a-kind? [hand]
  (let [freqs (vals (frequencies (map rank hand)))]
    (< 3 (apply max freqs)))
  )

(defn flush? [hand]
  (let [freqs (vals (frequencies (map suit hand)))]
    (== 5 (apply max freqs)))
  )

(defn full-house? [hand]
  (let [freqs (vals (frequencies (map rank hand)))]
    (cond
      (and (== 3 (apply max freqs))
           (== 2 (count freqs))) true
      :else false
      )
    )
  )

(defn two-pairs? [hand]
  (let [freqs (vals (frequencies (map rank hand)))]
    (and (== 2 (apply max freqs))
         (== 3 (count freqs))))
  )

(defn straight? [hand]
  (let [sorted-hand (vec (sort (map rank hand)))
        least (get sorted-hand 0)
        greatest (get sorted-hand 4)]
    (cond
      (= (range least (+ 5 least)) sorted-hand) true
      (= [2 3 4 5 14] sorted-hand) true
      :else false
      )
    )
  )

(defn straight-flush? [hand]
  (and (straight? hand)
       (flush? hand))
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
