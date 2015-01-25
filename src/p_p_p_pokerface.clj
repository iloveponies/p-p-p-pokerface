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
  (= '(2 3)
     (sort (rank-counts hand))))

(defn two-pairs? [hand]
  (= '(1 2 2)
     (sort (rank-counts hand))))

(defn straight? [hand]
  (let [ranks    (sort (map rank hand))
        min-rank (apply min ranks)]
    (or (= (range min-rank (+ 5 min-rank))
           ranks)
        (= '(2 3 4 5 14)
           ranks))))

(defn straight-flush? [hand]
  nil)

(defn value [hand]
  nil)
