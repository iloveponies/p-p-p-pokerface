(ns p-p-p-pokerface)

(defn rank [card]
  (let [high-vals {\T 10
                   \J 11
                   \Q 12
                   \K 13
                   \A 14}
        [r _] card]
    (if (Character/isDigit r)
      (Integer/valueOf (str r))
      (high-vals r))))

(defn suit [card]
  (let [[_ s] card]
    (str s)))

(defn n-of-a-kind? [hand n]
  (let [ranks (map rank hand)
        freqs (frequencies ranks)]
    (= n (apply max (vals freqs)))))

(defn pair? [hand]
  (n-of-a-kind? hand 2))

(defn three-of-a-kind? [hand]
  (n-of-a-kind? hand 3))

(defn four-of-a-kind? [hand]
  (n-of-a-kind? hand 4))

(defn flush? [hand]
  (let [suits (map suit hand)
        freqs (frequencies suits)]
    (= (count hand)
       (apply max (vals freqs)))))

(defn full-house? [hand]
  (let [ranks (map rank hand)
        freqs (frequencies ranks)]
    (= [2 3] (sort (vals freqs)))))

(defn two-pairs? [hand]
  (let [ranks (map rank hand)
        freqs (frequencies ranks)]
    (or
     (= [1 2 2] (sort (vals freqs)))
     (four-of-a-kind? hand))))

(defn straight? [hand]
  (let [hmm? (fn [ranks]
               (let [ranks (sort ranks)
                     low (first ranks)]
                 (= ranks
                    (take (count ranks) (iterate inc low)))))
        aces-high-ranks (map rank hand)
        aces-low-ranks (map (fn [r] (if (= r 14) 1 r)) aces-high-ranks)]
    (or (hmm? aces-high-ranks)
        (hmm? aces-low-ranks))))

(defn straight-flush? [hand]
  (and (straight? hand)
       (flush? hand)))

(defn value [hand]
  (let [high-card? (fn [hand] true)
        checkers #{[high-card? 0]
                   [pair? 1]
                   [two-pairs? 2]
                   [three-of-a-kind? 3]
                   [straight? 4]
                   [flush? 5]
                   [full-house? 6]
                   [four-of-a-kind? 7]
                   [straight-flush? 8]}
        pairs (filter (fn [[checker? _]] (checker? hand))
                      checkers)
        vals (map second pairs)]
    (apply max vals)))
