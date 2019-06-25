(ns p-p-p-pokerface)

(defn rank [[rank _]]
  (let[replacements {\T 10, \J 11, \Q 12, \K 13, \A 14}]
      (if (Character/isDigit rank)
        (Integer/valueOf (str rank))
        (replacements rank))
      ))

(defn suit [[_ suit]]
  (str suit))

(defn sorted-rank-freq [hand]
  (sort (vals (frequencies (map rank hand)))))

(defn sorted-suit-freq [hand]
  (sort (vals (frequencies (map suit hand)))))

(defn n-of-a-kind? [hand n]
  (let [ranks (map rank hand)
        freqs (frequencies ranks)]
    (= (some #{n} (vals freqs)) n)))

(defn pair? [hand]
  (n-of-a-kind? hand 2))

(defn three-of-a-kind? [hand]
  (n-of-a-kind? hand 3))

(defn four-of-a-kind? [hand]
  (n-of-a-kind? hand 4))

(defn flush? [hand]
  (== (first (sorted-suit-freq hand)) 5))

(defn full-house? [hand]
  (= (sorted-rank-freq hand) (range 2 4)))

(defn two-pairs? [hand]
  (let [ranks (sorted-rank-freq hand)]
    (or (= ranks '(1 2 2))
        (= ranks '(1 4)))))

(defn straight? [hand]
  (let [ranks (sort (map rank hand))
        low_straight (sort (replace {14 1} ranks))
        min_rank (apply min ranks)
        higher_straight (map (fn [rank] (- rank min_rank)) ranks)]
    (or (= low_straight (range 1 6))
        (= higher_straight (range 0 5)))))

(defn straight-flush? [hand]
  (and (straight? hand)
       (flush? hand)))

(defn high-card? [hand]
  true) ; All hands have a high card.

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}
        values (map (fn [[checker value]] (if (checker hand) value 0)) checkers)]
    (apply max values)))
