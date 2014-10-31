(ns p-p-p-pokerface)

(defn rank [card]
  (let [[r _] card
        highcards {\T 10 \J 11 \Q 12 \K 13 \A 14}]
    (if (Character/isDigit r)
      (Integer/valueOf (str r))
      (get highcards r))))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn ofakind? [hand n]
  (>= (apply max (vals (frequencies (map rank hand)))) n))

(defn pair? [hand]
  (ofakind? hand 2))

(defn three-of-a-kind? [hand]
  (ofakind? hand 3))

(defn four-of-a-kind? [hand]
  (ofakind? hand 4))

(defn flush? [hand]
  (apply = (map suit hand)))

(defn full-house? [hand]
  (=  [2 3] (sort (vals (frequencies (map rank hand))))))

(defn two-pairs? [hand]
  (contains? #{[1 2 2] [2 3] [1 4] [5]} (sort (vals (frequencies (map rank hand))))))

(defn straight? [hand]
  (let [ace-high (sort (map rank hand))
        ace-low (sort (replace {14 1} ace-high ))
        rawstraight? (fn [h]
                      (let [lowest (apply min h)]
                        (= h (range lowest (+ lowest 5)))))]
    (or (rawstraight? ace-high) (rawstraight? ace-low))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                   [two-pairs? 2]  [three-of-a-kind? 3]
                   [straight? 4]   [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}]
    (apply max (map (fn [c]
                      (if ((first c) hand)
                        (second c)
                        0)) checkers))))
