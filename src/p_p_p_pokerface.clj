(ns p-p-p-pokerface)

(defn rank [[rank]]
  (let [high-cards {\T 10
                    \J 11
                    \Q 12
                    \K 13
                    \A 14}]
    (if (Character/isDigit rank)
      (Integer/valueOf (str rank))
      (high-cards rank))))

(defn suit [[_ suit]]
  (str suit))

(defn correct-frequency? [hand mapping desired-frequency]
  (let [hand-frequencies (frequencies (map mapping hand))
        hand-highest-frequency (apply max (vals hand-frequencies))]
    (== hand-highest-frequency desired-frequency)))

(defn pair? [hand]
  (correct-frequency? hand rank 2))

(defn three-of-a-kind? [hand]
  (correct-frequency? hand rank 3))

(defn four-of-a-kind? [hand]
  (correct-frequency? hand rank 4))

(defn flush? [hand]
  (correct-frequency? hand suit 5))

(defn has-frequencies? [hand frequency-a frequency-b]
  (let [rank-frequencies (frequencies (map rank hand))
        rank-frequencies-vals (vals rank-frequencies)]
    (and (== (first rank-frequencies-vals) frequency-a)
         (== (first (rest rank-frequencies-vals)) frequency-b))))

(defn full-house? [hand]
  (has-frequencies? hand 3 2))

(defn two-pairs? [hand]
  (has-frequencies? hand 2 2))

(defn straight? [hand]
  (let [hand-sorted (sort (map rank hand))
        lowest-rank (first hand-sorted)
        highest-rank (last hand-sorted)]
    (if (and (== lowest-rank 2) (== highest-rank 14))
      (let [hand-sorted-low-ace (sort (replace {14 1} hand-sorted))
            desired-low-ace-straight (range 1 6)]
        (= hand-sorted-low-ace desired-low-ace-straight))
      (let [desired-straight (range lowest-rank (+ highest-rank 1))]
        (= hand-sorted desired-straight)))))

(defn straight-flush? [hand]
  (and (flush? hand) (straight? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}
        passed-checkers (filter #((first %) hand) checkers)
        highest-value (apply max (map #(second %) passed-checkers))]
    highest-value))
