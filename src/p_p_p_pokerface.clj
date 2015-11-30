(ns p-p-p-pokerface)

(defn rank [card]
  (def replacements {\T 10, \J 11, \Q 12, \K 13, \A 14})
  (let [[fst _] card]
    (if (Character/isDigit fst) (Integer/valueOf (str fst)) (replacements fst))
    ))

(defn suit [card]
  (let [[fst snd] card]
    (str snd)
    ))

(defn pair? [hand]
  (let [ranks (map rank hand)]
    (>= (apply max (vals (frequencies ranks))) 2)
    ; apply frequencies, then minimum from that
    ; how to get vector of ranks?
    )
  )

(defn three-of-a-kind? [hand]
  (let [ranks (map rank hand)]
    (>= (apply max (vals (frequencies ranks))) 3)
    ))

(defn four-of-a-kind? [hand]
  (let [ranks (map rank hand)]
    (>= (apply max (vals (frequencies ranks))) 4)
    ))

(defn flush? [hand]
  (let [suits (map suit hand)]
    (= (apply max(vals (frequencies suits))) 5)
    ))

(defn full-house? [hand]
  (let [ranks (map rank hand) model (range 2 4)]
    (= (sort (vals (frequencies ranks))) model)
    ))

(defn two-pairs? [hand]
  (let [ranks (map rank hand)]
    (or (= (sort (vals (frequencies ranks))) (seq [1 2 2]))
        (= (sort (vals (frequencies ranks))) (seq [1 4])))
    ))


(defn straight? [hand]
  (let [ranks (map rank hand)
        lowest (apply min ranks)
        alt_ranks (replace {14 1} (map rank hand))
        ]
    (or (= (sort ranks) (range lowest (+ lowest 5)))
        (= (sort alt_ranks) (range 1 6)))
    ))

(defn straight-flush? [hand]
  (and (flush? hand) (straight? hand)))

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
