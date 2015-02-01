(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rank _] card
        rank-value {\T 10, \J 11, \Q 12, \K 13, \A 14}]
    (if (Character/isDigit rank)
      (Integer/valueOf (str rank))
      (rank-value rank))))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn high-card? [hand] true)

(defn pair? [hand]
  (let [ranks (map rank hand)]
    (= (apply max (vals (frequencies ranks))) 2)))

(defn three-of-a-kind? [hand]
  (let [ranks (map rank hand)]
    (= (apply max (vals (frequencies ranks))) 3)))

(defn four-of-a-kind? [hand]
  (let [ranks (map rank hand)]
    (= (apply max (vals (frequencies ranks))) 4)))

(defn flush? [hand]
  (let [suits (map suit hand)]
    (= (apply max (vals (frequencies suits))) 5)))

(defn full-house? [hand]
  (let [ranks (map rank hand)
        freq (set (vals (frequencies ranks)))]
    (and (contains? freq 2)
         (contains? freq 3))))

(defn two-pairs? [hand]
  (let [ranks (map rank hand)
        freqs (vals (frequencies ranks))]
    (= (count (filter #(= 2 %) freqs)) 2)))

(defn straight? [hand]
  (let [ranks (sort (map rank hand))]
    (or
      (= (range (first ranks) (+ (last ranks) 1)) ranks)
      (let [ranks (sort (replace {14 1} ranks))]
        (= (range (first ranks) (+ (last ranks) 1)) ranks)))))

(defn straight-flush? [hand]
  (and (straight? hand)
       (flush? hand)))

(defn value [hand]
  (let [checkers #{[high-card? 0] [pair? 1]
                   [two-pairs? 2] [three-of-a-kind? 3]
                   [straight? 4] [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}
        predicate (fn [pair]
                   (let [[checker _] pair] (checker hand)))]
    (apply max (map second (filter predicate checkers)))))
