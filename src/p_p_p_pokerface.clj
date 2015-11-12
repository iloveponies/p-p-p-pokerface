(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rank suit] card]
    (if (Character/isDigit rank)
        (Integer/valueOf (str rank))
        ({\T 10, \J 11, \Q 12, \K 13, \A 14} rank)
      )))

(defn suit [card]
  (let [[rank suit] card]
    (str suit)))

(defn number-of-kinds [hand]
  (let [freqs (frequencies (mapv rank hand))
        how-many-of-a-kind (vals freqs)
        max-number-of-kinds (apply max how-many-of-a-kind)]
    max-number-of-kinds))

(defn pair? [hand]
  (>= (number-of-kinds hand) 2))

(defn three-of-a-kind? [hand]
  (>= (number-of-kinds hand) 3))

(defn four-of-a-kind? [hand]
  (= (number-of-kinds hand) 4))

(defn flush? [hand]
  (apply = (map suit hand)))

(defn full-house? [hand]
  (let [freqs (frequencies (mapv rank hand))
        how-many-of-a-kind (vals freqs)]
    (= (sort how-many-of-a-kind) [2 3])))

(defn two-pairs? [hand]
  (let [freqs (frequencies (mapv rank hand))
        how-many-of-a-kind (sort (vals freqs))]
    (or (= how-many-of-a-kind [1 2 2]) (= how-many-of-a-kind [1 4]))))

(defn three-pairs? [hand]
  (let [freqs (frequencies (mapv rank hand))
        how-many-of-a-kind (vals freqs)]
    (= (sort how-many-of-a-kind) [2 2 1])))

(defn straight? [hand]
  (let [ranks (map rank hand)
        sorted-hand (sort ranks)
        smallest (apply min sorted-hand)
        straight-line (range smallest (+ smallest 5))
        sorted-hand-with-ace-1 (sort (replace {14, 1} ranks))]
    (or (= sorted-hand straight-line)
        (= sorted-hand-with-ace-1 (range 1 6)))))

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
        winning-checkers
                 (filter (fn [h]
                           (apply (first h) [hand]))
                         checkers)
        values   (map (fn [h]
                        (second h))
                      winning-checkers)]
    (apply max values)))
