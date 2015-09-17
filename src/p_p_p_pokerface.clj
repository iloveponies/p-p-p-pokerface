(ns p-p-p-pokerface)

(def replacements {\T 10, \J 11, \Q 12, \K 13, \A 14})

(defn rank [[fst _]]
  (if (Character/isDigit fst)
    (Integer/valueOf (str fst))
    (get replacements fst)))

(defn suit [[_ snd]]
  (str snd))

(defn rank-freqs [hand]
  (frequencies (map rank hand)))

(defn n-of-a-kind? [n hand]
  (contains? (set (vals (rank-freqs hand))) n))

(defn pair? [hand]
  (n-of-a-kind? 2 hand))

(defn three-of-a-kind? [hand]
  (n-of-a-kind? 3 hand))

(defn four-of-a-kind? [hand]
  (n-of-a-kind? 4 hand))

(defn flush? [hand]
  (= (count (set (map suit hand))) 1))

(defn full-house? [hand]
  (= (sort (set (vals (rank-freqs hand)))) '(2 3)))

(defn two-pairs? [hand]
  (let [freq-vals (vals (rank-freqs hand))
        pair-vals (filter #(= % 2) freq-vals)]
    (= (count pair-vals) 2)))

(defn continuous? [nums]
  (let [smallest (apply min nums)
        minrange (range smallest (+ smallest (count nums)))]
    (= minrange (sort nums))))

(defn straight? [hand]
  (let [ranks (map rank hand)
        ranks-replaced (replace {14 1} (map rank hand))]
    (or (continuous? ranks) (continuous? ranks-replaced))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  true) ; All hands have a high card.

(defn value [hand]
  (let [checkers #{[high-card? 0]   [pair? 1]
                   [two-pairs? 2]   [three-of-a-kind? 3]
                   [straight? 4]    [flush? 5]
                   [full-house? 6]  [four-of-a-kind? 7]
                   [straight-flush? 8]}
        checker-value (fn [[func n]]
                        (if (func hand)
                          n
                          -1))]
    (apply max (map checker-value checkers))))
