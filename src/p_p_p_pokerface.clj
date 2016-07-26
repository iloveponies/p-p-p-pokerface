(ns p-p-p-pokerface)

(defn rank [card]
  (def replacements {\T 10, \J 11, \Q 12, \K 13, \A 14})
  (let [[fst _] card]
    (cond
      (Character/isDigit fst) (Integer/valueOf (str fst))
      :else (replacements fst))))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn high-card? [hand]
  true)

(defn pair? [hand]
  (>= (apply max (vals (frequencies (map rank hand)))) 2))

(defn three-of-a-kind? [hand]
  (>= (apply max (vals (frequencies (map rank hand)))) 3))

(defn four-of-a-kind? [hand]
  (>= (apply max (vals (frequencies (map rank hand)))) 4))

(defn flush? [hand]
  (= (apply max (vals (frequencies (map suit hand)))) 5))

(defn full-house? [hand]
  (= (sort (vals (frequencies (map rank hand)))) (range 2 4)))

(defn two-pairs? [hand]
  (= (sort (vals (frequencies (map rank hand)))) (seq [1 2 2])))

(defn straight? [hand]
  (let [ranks (sort (map rank hand))]
    (or
      (= ranks (range (apply min ranks) (+ 1 (apply max ranks))))
      (= (sort (replace {14 1} ranks)) (range (apply min (replace {14 1} ranks)) (+ 1 (apply max (replace {14 1} ranks)))))
  )))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}]
  (apply max (map  second (filter (fn [x] ((first x) hand)) checkers)))))
