(ns p-p-p-pokerface)

(defn rank [[r]]
  (if (Character/isDigit r)
    (Integer/valueOf (str r))
    (get {\T 10 \J 11 \Q 12 \K 13 \A 14} r)))

(defn suit [[_ s]]
  (str s))

(defn n-of-a-kind? [hand n]
  (contains? (set (vals (frequencies (map rank hand)))) n))

(defn sorted-rank-sequence [hand]
  (sort (vals (frequencies (map rank hand)))))

(defn pair? [hand]
  (n-of-a-kind? hand 2))

(defn three-of-a-kind? [hand]
  (n-of-a-kind? hand 3))

(defn four-of-a-kind? [hand]
  (n-of-a-kind? hand 4))

(defn two-pairs? [hand]
  (or (four-of-a-kind? hand)
      (= '(1 2 2) (sorted-rank-sequence hand))))

(defn straight? [hand]
  nil)

(defn flush? [hand]
  (apply = (map suit hand)))

(defn full-house? [hand]
  (= '(2 3) (sorted-rank-sequence hand)))

(defn straight-flush? [hand]
  nil)

(defn value [hand]
  nil)
