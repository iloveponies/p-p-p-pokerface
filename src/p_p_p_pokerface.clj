(ns p-p-p-pokerface)

(def replacements {\T 10, \J 11, \Q 12, \K 13, \A 14})

(defn rank [card]
  (let [[r _] card]
    (cond
     (Character/isDigit r) (Integer/valueOf (str r))
     :else (replacements r))))

(defn suit [card]
  (let [[_ st] card]
    (str st)))

(defn n-of-a-kind [hand n]
  (let [ranks (map rank hand)]
    (let [freqs (vals (frequencies ranks))]
      (>= (apply max freqs) n))))

(defn pair? [hand]
  (n-of-a-kind hand 2))

(defn three-of-a-kind? [hand]
  (n-of-a-kind hand 3))

(defn four-of-a-kind? [hand]
  (n-of-a-kind hand 4))

(defn same-suit? [hand]
  (let [suits (map suit hand)]
    (let [freqs (vals (frequencies suits))]
      (= 1 (count freqs)))))

(defn flush? [hand]
  (let [ranks (map rank hand)]
    (let [minrank (apply min ranks)]
      (let [ranks-in-order (= (sort ranks) (range minrank (+ minrank 5)))]
        (and (same-suit? hand) (not ranks-in-order))))))

(defn full-house? [hand]
  (let [ranks (map rank hand)]
    (let [freqs (sort (vals (frequencies ranks)))]
      (= (seq [2 3]) freqs))))

(defn two-pairs? [hand]
  (let [ranks (map rank hand)]
    (let [freqs (sort (vals (frequencies ranks)))]
      (or (= (seq [1 2 2]) freqs) (= (seq [1 4]) freqs)))))

(defn possible-rank-range [ranks]
  (let [minrank (apply min ranks)]
    (range minrank (+ minrank 5))))

(defn straight? [hand]
  (let [ranks (map rank hand)]
    (let [ace-as-1 (sort (replace {1 14} ranks))]
      (let [ace-as-14 (sort (replace {14 1} ranks))]
        (or (= ace-as-1 (possible-rank-range ace-as-1))
            (= ace-as-14 (possible-rank-range ace-as-14)))))))

(defn straight-flush? [hand]
  (and (straight? hand) (same-suit? hand)))

(defn high-card? [hand]
  true) ; All hands have a high card.

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}]
    (let [allmatches (map (fn [[func, value]] (if (func hand) value nil)) checkers)]
      (let [matches (filter boolean allmatches)]
        (apply max matches)))))

(defn hand [a-hand]
  a-hand)

(defn card [a-card]
  a-card)
