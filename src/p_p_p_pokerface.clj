(ns p-p-p-pokerface)

(defn rank [card]
  (let [[r _] card
        values {\T 10, \J 11, \Q 12, \K 13, \A 14}]
    (if (Character/isDigit r)
      (Integer/valueOf (str r))
      (get values r))))

(defn suit [card]
    (let [[_ s] card]
    (str s)))

(defn rank-frequency [hand]
  (vals (frequencies (map rank hand))))

(defn pair? [hand]
  (<= 2 (apply max (rank-frequency hand))))

(defn three-of-a-kind? [hand]
  (<= 3 (apply max (rank-frequency hand))))

(defn four-of-a-kind? [hand]
  (<= 4 (apply max (rank-frequency hand))))

(defn flush? [hand]
  (= 1 (count (vals (frequencies (map suit hand))))))

(defn full-house? [hand]
  (let [freqs (rank-frequency hand)]
    (and
     (= 3 (apply max freqs))
     (= 2 (apply min freqs)))))

(defn two-pairs? [hand]
  (let [freqs (frequencies (rank-frequency hand))]
    (and
     (contains? freqs 2)
     (<= 2 (get freqs 2)))))

(defn straight? [hand]
  (let [in-order?
        (fn [vals]
          (let [sorted (sort vals)
                first-rank (first sorted)
                correct (range first-rank (+ first-rank (count sorted)))]
            (= sorted correct)))]
    (or
     (in-order? (map rank hand))
     (in-order? (replace {14 1} (map rank hand))))))

(defn straight-flush? [hand]
  (and
   (straight? hand)
   (flush? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                   [two-pairs? 2]  [three-of-a-kind? 3]
                   [straight? 4]   [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}
        hands (filter (fn [checker] ((first checker) hand)) checkers)]
    (apply max (map second hands))))
