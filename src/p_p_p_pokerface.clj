(ns p-p-p-pokerface)

(def high-rank
  {\T 10
   \J 11
   \Q 12
   \K 13
   \A 14})

(defn rank [card]
  (let [[r _] card]
    (or (get high-rank r)
        (Integer/valueOf (str r)))))

(defn suit [card]
  (let [[_ s] card]
    (str s)))

(defn rank-freqs [hand]
  (frequencies (map rank hand)))

(defn rank-counts [hand]
  (vals (rank-freqs hand)))

(defn pair? [hand]
  (== (apply max (rank-counts hand))
      2))

(defn three-of-a-kind? [hand]
  (== (apply max (rank-counts hand))
      3))

(defn four-of-a-kind? [hand]
  (== (apply max (rank-counts hand))
      4))

(defn flush? [hand]
  (== (count (frequencies (map suit hand)))
      1))

(defn full-house? [hand]
  (let [hand-counts (rank-counts hand)
        max-count   (apply max hand-counts)
        min-count   (apply min hand-counts)]
    (and (== max-count 3)
         (== min-count 2))))

(defn two-pairs? [hand]
  nil)

(defn straight? [hand]
  nil)

(defn straight-flush? [hand]
  nil)

(defn value [hand]
  nil)
