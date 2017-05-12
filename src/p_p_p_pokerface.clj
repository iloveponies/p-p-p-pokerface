(ns p-p-p-pokerface)

(defn rank [card]
  (def ranks {\2 2, \3 3, \4 4, \5 5, \6 6, \7 7, \8 8, \9 9, \T 10, \J 11, \Q 12, \K 13, \A 14})
  (get ranks (get card 0)))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn pair? [hand]
  (>= (apply max (vals (frequencies (map rank hand)))) 2))

(defn three-of-a-kind? [hand]
  (>= (apply max (vals (frequencies (map rank hand)))) 3))

(defn four-of-a-kind? [hand]
  (>= (apply max (vals (frequencies (map rank hand)))) 4))

(defn flush? [hand]
  (>= (apply max (vals (frequencies (map suit hand)))) 5))

(defn full-house? [hand]
  (= [3 2] (vals (frequencies (map rank hand)))))

(defn two-pairs? [hand]
  (or (= [2 2 1] (vals (frequencies (map rank hand))))
      (= [4 1] (vals (frequencies (map rank hand))))))

(defn straight? [hand]
  (let [sorthand (sort (map rank hand))
        sorthand_la (sort (replace {14 1} (map rank hand)))
        fst (first sorthand)
        fst_la (first sorthand_la)]
    (or (= (range fst (+ fst 5)) sorthand)
        (= (range fst_la (+ fst_la 5)) sorthand_la))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                   [two-pairs? 2]  [three-of-a-kind? 3]
                   [straight? 4]   [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}
        ]
    (apply max (map second (filter (fn [checker]
              ((first checker) hand))
            checkers)))))

(defn hand [a-hand]
  a-hand)

(defn card [a-card]
  a-card)
