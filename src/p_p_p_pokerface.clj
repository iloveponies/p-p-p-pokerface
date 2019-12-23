(ns p-p-p-pokerface)

(def replacements {\T 10, \J 11, \Q 12, \K 13, \A 14})

(defn rank [card]
  (let [[fst _] card]
  (if (Character/isDigit fst)
    (Integer/valueOf (str fst))
    (replacements fst))))

(defn suit [card]
  (let [[_ snd] card]
  (str snd)))

(defn pair? [hand]
  (contains? (set(vals (frequencies (map rank hand) ))) 2))

(defn three-of-a-kind? [hand]
  (contains? (set(vals (frequencies (map rank hand) ))) 3))

(defn four-of-a-kind? [hand]
  (contains? (set(vals (frequencies (map rank hand) ))) 4))

(defn flush? [hand]
  (contains? (set(vals (frequencies (map suit hand) ))) 5))

(defn full-house? [hand]
  (and
    (contains? (set(vals (frequencies (map rank hand) ))) 2)
    (contains? (set(vals (frequencies (map rank hand) ))) 3)))


(defn two-pairs? [hand]
  (and
    (contains? (set(vals (frequencies (map rank hand) ))) 2)
    (= (count (vals (frequencies (map rank hand) ))) 3) ))

(defn straight? [hand]
 (let [ fst (first(sort (map rank hand)))
        hand2 (sort (replace {"AC" "1C", "AD" "1D","AH" "1H","AS" "1S"} hand))]
  (or
    (= (sort (map rank hand)) (range fst (+ fst 5)))
    (=  (map rank hand2) (range (first (map rank hand2)) (+ (first (map rank hand2)) 5)))

    )
 ))

(defn straight-flush? [hand]
  (and
    (straight? hand)
    (flush? hand)
    ))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}
        matching_hands (map (fn[x][((first x ) hand ) (second x)]) checkers)
        ]

     (apply max(map second(filter first matching_hands ) ))
    ))


