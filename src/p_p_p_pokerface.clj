(ns p-p-p-pokerface)

(defn rank [card]
  (let [[c _] card
        replacements {\T 10 \J 11 \Q 12 \K 13 \A 14}]
  (cond
    (Character/isDigit c) (Integer/valueOf (str c))
    :else (replacements c)
    )))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn pair? [hand]
  (contains? (set (vals (frequencies (map rank hand)))) 2))

(defn three-of-a-kind? [hand]
  (contains? (set (vals (frequencies (map rank hand)))) 3))

(defn four-of-a-kind? [hand]
  (contains? (set (vals (frequencies (map rank hand)))) 4))

(defn flush? [hand]
  (contains? (set (vals (frequencies (map suit hand)))) 5))

(defn full-house? [hand]
  (let [sorted-freq (sort (vals (frequencies (map rank hand))))]
    (= [2 3] sorted-freq)))

(defn two-pairs? [hand]
  (or (four-of-a-kind? hand) (== (count (filter (fn [x] (== x 2)) (vals (frequencies (map rank hand))))) 2)))

(defn straight? [hand]
  (let [ranks (map rank hand)
        low-ace-ranks (replace {14 1} ranks)]
    (cond
      (pair? hand) false
      (or (== (- (apply max ranks) (apply min ranks)) 4) (== (- (apply max low-ace-ranks) (apply min low-ace-ranks)) 4)) true
      :else false
      )))

(defn straight-flush? [hand]
  (and (flush? hand) (straight? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]
                  [pair? 1]
                  [two-pairs? 2]
                  [three-of-a-kind? 3]
                  [straight? 4]
                  [flush? 5]
                  [full-house? 6]
                  [four-of-a-kind? 7]
                  [straight-flush? 8]}]

    (apply max (map second (filter (fn [x] ((first x) hand)) checkers )))))
