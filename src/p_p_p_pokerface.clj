(ns p-p-p-pokerface)

(defn rank [card]
  (let [[r _] card
        code {\T 10, \J 11, \Q 12, \K 13, \A 14}]
    (if (Character/isDigit r)
      (Integer/valueOf (str r))
      (get code r))))

(defn suit [card]
  (let [[_ s] card]
    (str s)))

(defn rank-frequencies [hand]
  (vals (frequencies (map rank hand))))

(defn most-of-a-kind [n hand]
  (< (- n 1) (apply max (rank-frequencies hand))))

(defn high-card? [hand]
  true)

(defn pair? [hand]
  (most-of-a-kind 2 hand))

(defn three-of-a-kind? [hand]
  (most-of-a-kind 3 hand))

(defn four-of-a-kind? [hand]
  (most-of-a-kind 4 hand))

(defn flush? [hand]
  (let [suits (map suit hand)
        s (first suits)]
    (every? #(= s %) suits)))

(defn full-house? [hand]
  (let [rf (rank-frequencies hand)
        srf (sort rf)]
    (= [2 3] srf)))

(defn two-pairs? [hand]
  (let [rf (rank-frequencies hand)
        rff (frequencies rf)]
    (or (= 2 (get rff 2))
        (four-of-a-kind? hand))))

(defn straight?-impl [ranks]
  (let [sr (sort ranks)
        min (apply min ranks)
        check (seq (range min (+ min 5)))]
    (= check sr)))

(defn straight? [hand]
  (let [ranks (map rank hand)
        aces-low (replace {14 1 1 14} ranks)]
    (or (straight?-impl ranks)
        (straight?-impl aces-low))))

(defn straight-flush? [hand]
  (and (flush? hand)
       (straight? hand)))

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                   [two-pairs? 2]  [three-of-a-kind? 3]
                   [straight? 4]   [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}]
    (apply max (map second (filter #((first %) hand) checkers)))))
