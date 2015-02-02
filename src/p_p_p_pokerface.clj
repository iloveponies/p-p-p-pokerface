(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rank _] card
        replacements {\T 10
                      \J 11
                      \Q 12
                      \K 13
                      \A 14}]
    (if (Character/isDigit rank)
      (Integer/valueOf (str rank))
      (replacements rank))))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn high-card? [hand]
  true)

(defn pair? [hand]
  (contains? (set (vals (frequencies (map rank hand)))) 2))

(defn three-of-a-kind? [hand]
  (contains? (set (vals (frequencies (map rank hand)))) 3))

(defn four-of-a-kind? [hand]
  (contains? (set (vals (frequencies (map rank hand)))) 4))

(defn flush? [hand]
  (contains? (set (vals (frequencies (map suit hand)))) 5))

(defn full-house? [hand]
  (and (pair? hand) (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  (if (pair? hand)
    (== (count (vals (frequencies (map rank hand)))) 3)
    false))

(defn straight? [hand]
  (let [ranked-hand              (sort (map rank hand))
        ranked-hand-min          (first ranked-hand)
        ace-replaced-hand        (sort (replace {14 1} ranked-hand))
        ace-replaced-hand-min    (first ace-replaced-hand)]
    (or
      (= ranked-hand (range ranked-hand-min (+ ranked-hand-min 5)))
      (= ace-replaced-hand (range ace-replaced-hand-min (+ ace-replaced-hand-min 5))))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn value [hand]
  (let [checkers      #{[high-card? 0]  [pair? 1]
                        [two-pairs? 2]  [three-of-a-kind? 3]
                        [straight? 4]   [flush? 5]
                        [full-house? 6] [four-of-a-kind? 7]
                        [straight-flush? 8]}]
    (apply max (map second (filter (fn [x] ((first x) hand)) checkers)))))
