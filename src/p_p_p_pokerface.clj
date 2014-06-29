(ns p-p-p-pokerface)

(def replacements {\T 10
                   \J 11
                   \Q 12
                   \K 13
                   \A 14})

(defn rank [card]
  (let [[r _] card]
    (if (Character/isDigit r)
      (Integer/valueOf (str r))
      (replacements r))))

(defn suit [card]
  (let [[_ s] card]
    (str s)))

(defn pair? [hand]
  (let [freqs (vals (frequencies (map rank hand)))]
    (= 2 (apply max freqs))))

(defn three-of-a-kind? [hand]
  (let [freqs (vals (frequencies (map rank hand)))]
    (= 3 (apply max freqs))))

(defn four-of-a-kind? [hand]
  (let [freqs (vals (frequencies (map rank hand)))]
    (= 4 (apply max freqs))))

(defn flush? [hand]
  (let [freqs (vals (frequencies (map suit hand)))]
    (= 5 (apply max freqs))))

(defn full-house? [hand]
  (let [flush-seq (range 2 4)
        hand-seq (sort (vals (frequencies (map rank hand))))]
    (= flush-seq hand-seq)))

(defn two-pairs? [hand]
  (let [two-pair-seq (seq [1 2 2])
        two-pair-alt (seq [1 4])
        hand-seq (sort (vals (frequencies (map rank hand))))]
    (or (= two-pair-seq hand-seq)
        (= two-pair-alt hand-seq))))

(defn straight? [hand]
  (let [hand-seq (sort (map rank hand))
        min-rank (apply min hand-seq)]
    (or
       (= (sort (replace {14 1} hand-seq)) (seq [1 2 3 4 5]))
       (= hand-seq (range min-rank (+ min-rank 5))))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0] [pair? 1]
                   [two-pairs? 2] [three-of-a-kind? 3]
                   [straight? 4]  [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}
        matchers (filter (fn [[matcher value]] (matcher hand)) checkers)
        values (map second matchers)]
    (apply max values)))
