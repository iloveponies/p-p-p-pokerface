(ns p-p-p-pokerface)

(defn rank [[r _]]
  (let [rank-values {\T 10
                     \J 11
                     \Q 12
                     \K 13
                     \A 14}]
    (if (Character/isDigit r)
      (Integer/valueOf (str r))
      (rank-values r))))

(defn suit [[_ s]]
  (str s))

(defn pair-of? [n hand]
  (let [ranks (map rank hand)
        freqs (vals (frequencies ranks))]
    (>= (apply max freqs) n)))

(defn pair? [hand]
  (pair-of? 2 hand))

(defn three-of-a-kind? [hand]
  (pair-of? 3 hand))

(defn four-of-a-kind? [hand]
  (pair-of? 4 hand))

(defn flush? [hand]
  (let [suits (map suit hand)
        freqs (vals (frequencies suits))]
  (= (apply max freqs) 5)))

(defn full-house? [hand]
  (let [ranks (map rank hand)
        freqs (sort (vals (frequencies ranks)))]
    (= '(2 3) freqs)))

(defn two-pairs? [hand]
  (let [ranks (map rank hand)
        freqs (sort (vals (frequencies ranks)))]
    (or (= '(1 2 2) freqs)
        (= '(1 4) freqs))))

(defn straight? [hand]
  nil)

(defn straight-flush? [hand]
  nil)

(defn value [hand]
  nil)
