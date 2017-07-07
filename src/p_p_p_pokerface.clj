(ns p-p-p-pokerface)

(def replacements {\1 1, \2 2, \3 3, \4 4, \5 5, \6 6, \7 7
                   \8 8, \9 9, \T 10, \J 11, \Q 12, \K 13, \A 14})

(defn rank [[r _]]
  (get replacements r))

(defn suit [[_ s]]
  (str s))

(defn pair? [hand]
  (if (some (fn [x] (> x 1)) (vals (frequencies (map rank hand))))
    true
    false))

(defn three-of-a-kind? [hand]
  (if (some (fn [x] (> x 2)) (vals (frequencies (map rank hand))))
    true
    false))

(defn four-of-a-kind? [hand]
  (if (some (fn [x] (> x 3)) (vals (frequencies (map rank hand))))
    true
    false))

(defn flush? [hand]
  (apply = (map suit hand)))

(defn full-house? [hand]
  (let [counts (set (vals (frequencies (map rank hand))))]
    (and (contains? counts 2) (contains? counts 3))))

(defn two-pairs? [hand]
  (let [counts (vals (frequencies (map rank hand)))]
    (< 1 (count (filter (fn [x] (> x 1)) counts)))))

(defn seq-straight? [sequence]
  (let [delta (- (apply max sequence) (apply min sequence))]
    (== delta 4)))

(defn straight? [hand]
  (let [sorted-vals (sort (mapv rank hand))
        val-set (set sorted-vals)]
    (if (== (count val-set) 5)
      (if (contains? val-set 14)
        (or (seq-straight? sorted-vals) (seq-straight? (cons 1 (drop-last 1 sorted-vals))))
        (seq-straight? sorted-vals))
      false)))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

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
    :else 0))
