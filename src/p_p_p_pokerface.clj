(ns p-p-p-pokerface)

(defn rank [card]
  (let [[r] card
        high-values {\T 10, \J 11, \Q 12, \K 13, \A 14}
        is-digit? (Character/isDigit r)
        value (str (if is-digit? r (get high-values r)))]
    (Integer/valueOf value)))

(defn suit [card]
  (let [[_ s] card]
    (str s)))

(defn rank-distribution [hand]
  (vals (frequencies (map rank hand))))

(defn n-of-a-kind? [hand n]
  (let [freqs (rank-distribution hand)
        most-of-a-kind (apply max freqs)]
    (>= most-of-a-kind n)))

(defn pair? [hand]
  (n-of-a-kind? hand 2))


(defn three-of-a-kind? [hand]
  (n-of-a-kind? hand 3))

(defn four-of-a-kind? [hand]
  (n-of-a-kind? hand 4))

(defn flush? [hand]
  (let [suits (map suit hand)
        suits-in-hand (count (set suits))]
    (= suits-in-hand 1)))

(defn full-house? [hand]
  (= (sort (rank-distribution hand)) [2 3]))

(defn two-pairs? [hand]
  (or
    (four-of-a-kind? hand) ; four of a kind is two pairs
    (= (sort (rank-distribution hand)) [1 2 2]))) ; regular pairs

(defn straight? [hand]
  (let [ranks (sort (map rank hand))
        low (apply min ranks)
        high (apply max ranks)
        comparison (range low (inc high))]
    (or
      (= ranks comparison)
      (if (= high 14) ; value of ace can be 14 or 1 in a straight
        (let [new-ranks (cons 1 (drop-last ranks))
              new-low (apply min new-ranks)
              new-high (apply max new-ranks)
              new-comparison (range new-low (inc new-high))] ; remove the 14 from the end and prepend 1
          (= new-comparison new-ranks))
        false))))

(defn straight-flush? [hand]
  (and
    (flush? hand)
    (straight? hand)))

(defn high-card? [hand]
  true) ; every hand has a high card

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                   [two-pairs? 2]  [three-of-a-kind? 3]
                   [straight? 4]   [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}
        has-checker? (fn [checker] ((first checker) hand))]
    (apply max (map second (filter has-checker? checkers)))))
