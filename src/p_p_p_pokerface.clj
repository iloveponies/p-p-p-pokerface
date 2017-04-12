(ns p-p-p-pokerface)

(defn rank [card]
  (let [pictures {\T 10, \J 11, \Q 12, \K 13, \A 14}
        [rank _] card]
    (if (Character/isDigit rank)
      (Integer/valueOf (str rank))
      (Integer/valueOf (str (get pictures rank))))))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn pair? [hand]
  (let [ranks (fn [card] (rank card))
        freq (set (vals (frequencies (map ranks hand))))]
    (if (contains? freq 2) true false)))

(defn three-of-a-kind? [hand]
  (let [ranks (fn [card] (rank card))
        freq (vals (frequencies (map ranks hand)))]
    (if (= (apply max freq) 3) true false)))

(defn four-of-a-kind? [hand]
  (let [ranks (fn [card] (rank card))
        freq (vals (frequencies (map ranks hand)))]
    (if (= (apply max freq) 4) true false)))

(defn flush? [hand]
  (let [suits (fn [card] (suit card))
        freq (vals (frequencies (map suits hand)))]
    (if (= (apply max freq) 5) true false)))

(defn full-house? [hand]
  (if (and (pair? hand) (three-of-a-kind? hand)) true false))

(defn two-pairs? [hand]
  (let [ranks (fn [card] (rank card))
        rank-freq (vals (frequencies (map ranks hand)))
        freq (frequencies rank-freq)]
    (if (or (= (get freq 2) 2) (four-of-a-kind? hand)) true false)))

(defn straight? [hand]
  (let [ranks (sort (map (fn [card] (rank card)) hand))
        hand-range (range (apply min ranks) (+ (apply max ranks) 1))]
    (if (= (apply max ranks) 14)
      (let [mod-ranks (sort (replace {14 1} ranks))
            mod-hand-range (range (apply min mod-ranks) (+ (apply max mod-ranks) 1))]
        (if (or (= hand-range ranks) (= mod-hand-range mod-ranks)) true false))
      (if (= hand-range ranks) true false))))

(defn straight-flush? [hand]
  (if (and (flush? hand) (straight? hand)) true false))

(defn value [hand]
  (defn high-card? [hand]
    true)
  (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}
        points (map (fn [pair] (if ((first pair) hand) (second pair) 0)) checkers)]
    points
    (apply max points)))
