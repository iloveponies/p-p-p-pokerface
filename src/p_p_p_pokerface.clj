(ns p-p-p-pokerface)

(defn rank [card]
  (let [ranks {\1 1
               \2 2
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
               \A 14}
        [rank _] card]
    (get ranks rank)))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn pair? [hand]
  (let [not-empty? (complement empty?)]
    (->> hand
         (map rank)
         frequencies
         (filter
           (fn [[rank freqs]]
             (>= freqs 2)))
         not-empty?)))

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
