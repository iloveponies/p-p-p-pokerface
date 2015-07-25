(ns p-p-p-pokerface)

(defn high-card? [hand]
  true)                                                     ; All hands have a high card.

(defn rank [card]
  (let [[rank-character _] card
        ranks {\T 10
               \J 11
               \Q 12
               \K 13
               \A 14}]
    (if (Character/isDigit rank-character)
      (Integer/valueOf (str rank-character))
      (ranks rank-character))))

(defn suit [card]
  (let [[_ suit-letter] card]
    (str suit-letter)))

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
  (apply = (map suit hand)))

(defn full-house? [hand]
  (let [ranks (map rank hand)]
    (= [2 3] (sort (vals (frequencies ranks))))))

(defn two-pairs? [hand]
  (let [ranks (map rank hand)]
    (= [1 2 2] (sort (vals (frequencies ranks))))))

(defn straight? [hand]
  (let [ranks (map rank hand)
        straight2? (fn [ranks]
                     (let [freqs (vals (frequencies ranks))]
                       (and (= 1 (apply max freqs))
                            (= 4 (- (apply max ranks) (apply min ranks))))))]
    (or
      (straight2? (sort ranks))
      (let [ranks (sort (replace {14 1} ranks))]
        (straight2? ranks)))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

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
    (apply max (map second (filter (fn [check-vec]
                                     (let [[checker _] check-vec]
                                       (checker hand))) checkers)))))
