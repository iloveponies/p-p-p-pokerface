(ns p-p-p-pokerface)

(defn rank [card]
  (def replacements {"T" 10, "J" 11, "Q" 12, "K" 13, "A" 14})
  (let [[rank suit] card]
    (if (Character/isDigit rank)
      (Integer/valueOf (str rank))
      (replacements (str rank))
    )))

(defn suit [card]
  (let [[rank suit] card]
    (str suit)))

(defn pair? [hand]
  (= (apply max (vals (frequencies (map rank hand)))) 2))

(defn three-of-a-kind? [hand]
  (= (apply max (vals (frequencies (map rank hand)))) 3))

(defn four-of-a-kind? [hand]
  (= (apply max (vals (frequencies (map rank hand)))) 4))

(defn flush? [hand]
  (= (first (vals (frequencies (map suit hand)))) 5))

(defn full-house? [hand]
  (= (range 2 4) (sort (vals (frequencies (map rank hand))))))

(defn two-pairs? [hand]
  (= [1, 2, 2] (sort (vals (frequencies (map rank hand))))))

(defn straight? [hand]
  (def replacement {14 1})
  (def m (apply min (map rank hand)))
  (cond
    (and (= m 2) (= (sort (replace replacement (map rank hand))) (range 1 6))) true
    :else (= (sort (map rank hand)) (range m (+ m 5)))))

(defn straight-flush? [hand]
  (boolean (and (flush? hand) (straight? hand))))

(defn high-card? [hand] true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}]

    (apply max (map (fn [checker] (second checker)) (filter (fn [checker] ((first checker) hand)) checkers)))))

(defn hand [a-hand]
  a-hand)

(defn card [a-card]
  a-card)
