(ns p-p-p-pokerface)

(defn rank [[rank _]]
  (let [ranks {\2 2
               \3 3
               \4 4
               \5 5
               \6 6
               \7 7
               \8 8
               \9 9
               \T 10
               \J 11
               \Q 12
               \K 13
               \A 14}]
    (get ranks rank)))

(defn suit [[_ suit]]
  (str suit))

(defn n-of-a-kind? [n hand]
  (let [ranks (map rank hand)
        freqs (vals (frequencies ranks))
        max-freq (apply max freqs)]
    (>= max-freq n)))

(defn pair? [hand] (n-of-a-kind? 2 hand))

(defn three-of-a-kind? [hand] (n-of-a-kind? 3 hand))

(defn four-of-a-kind? [hand] (n-of-a-kind? 4 hand))

(defn flush? [hand]
  (let [distinct-suits (distinct (map suit hand))]
    (= 1 (count distinct-suits))))

(defn full-house? [hand]
  (let [ranks (map rank hand)
        freqs (vals (frequencies ranks))]
    (= [2 3] (sort freqs))))

(defn two-pairs? [hand]
  (let [ranks (map rank hand)
        sorted-freqs (sort (vals (frequencies ranks)))]
    (or
     (= [1 2 2] sorted-freqs)
     (= [1 4] sorted-freqs))))

(defn straight? [hand]
  (let [ranks (map rank hand)
        ranks-ace-low (replace {14 1} ranks)
        delta (- (apply max ranks) (apply min ranks))
        delta-ace-low (- (apply max ranks-ace-low) (apply min ranks-ace-low))
        ]
   (and (= 5 (count (distinct ranks)))
        (or (= 4 delta) (= 4 delta-ace-low)))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  true) ; All hands have a high card.

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}
        check (fn [[checker value]] (if (checker hand) value 0))
        values (map check checkers)]
    (apply max values)))
