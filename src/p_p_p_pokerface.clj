(ns p-p-p-pokerface)

(def replacement { \T 10, \J 11, \Q 12, \K 13, \A 14})

(defn rank [card]
  (let [[rank _] card]
    (if (Character/isDigit rank)
      (Integer/valueOf (str rank))
      (get replacement rank)
      )))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn rank-count [hand]
  (let [freqs (fn [hand](frequencies (map rank hand)))]
    (set (vals (freqs hand)))
    ))

(defn pair? [hand]
    (contains? (rank-count hand) 2))

(defn three-of-a-kind? [hand]
  (contains? (rank-count hand) 3))

(defn four-of-a-kind? [hand]
  (contains? (rank-count hand) 4))

(defn flush? [hand]
  (let [freqs (fn [hand](frequencies (map suit hand)))]
    (contains? (set (vals (freqs hand))) 5)))

(defn full-house? [hand]
  (and (pair? hand)
       (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  (let [freqs (fn [hand](frequencies (map rank hand)))]
    (or (contains? (rank-count hand) 4)
    (and (contains? (set (vals (frequencies (vals (freqs hand))))) 2)
         (pair? hand)))))

(defn straight? [hand]
  (let [alt-rank (fn [hand] (replace {14 1 } (map rank hand)))
        max-val (fn [hand] (apply max (map rank hand)))
        min-val (fn [hand] (apply min (map rank hand)))
        alt-max-val (fn [hand] (apply max (alt-rank hand)))
        alt-min-val (fn [hand] (apply min (alt-rank hand)))
        normal-sort (fn [hand] (sort (map rank hand)))
        alt-sort (fn [hand](sort (alt-rank hand)))
        straight-hand (fn [hand] (range (min-val hand) (+ (max-val hand) 1)))
        alt-straight-hand (fn [hand] (range (alt-min-val hand) (+ (alt-max-val hand) 1)))]
        (or (= (straight-hand hand) (normal-sort hand))
            (= (alt-straight-hand hand) (alt-sort hand)))))

(defn straight-flush? [hand]
  (and (straight? hand)
       (flush? hand)
       ))

(defn value [hand]
  (cond
   (straight-flush? hand) 8
   (four-of-a-kind? hand) 7
   (full-house? hand) 6
   (flush? hand) 5
   (straight? hand) 4
   (three-of-a-kind? hand) 3
   (two-pairs? hand) 2
   (pair? hand) 1
   :else 0
   ))
