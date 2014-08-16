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
  nil)

(defn four-of-a-kind? [hand]
  nil)

(defn flush? [hand]
  nil)

(defn full-house? [hand]
  nil)

(defn two-pairs? [hand]
  nil)

(defn straight? [hand]
  nil)

(defn straight-flush? [hand]
  nil)

(defn value [hand]
  nil)
