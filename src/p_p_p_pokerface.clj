(ns p-p-p-pokerface)

(defn rank [[r _]]
  (if (Character/isDigit r)
    (Integer/valueOf (str r))
    (get {\T 10, \J 11, \Q 12, \K 13, \A 14} r)))

(defn suit [[_ s]]
  (str s))

(defn pair? [hand]
  (>=
    (apply max
      (vals
        (frequencies
          (map rank hand))))
    2))

(defn three-of-a-kind? [hand]
  (>=
    (apply max
      (vals
        (frequencies
          (map rank hand))))
    3))

(defn four-of-a-kind? [hand]
  (>=
    (apply max
      (vals
        (frequencies
          (map rank hand))))
    4))

(defn flush? [hand]
  (apply = (map suit hand)))

(defn full-house? [hand]
  (=
    (sort
      (vals
        (frequencies
          (map rank hand))))
    [2 3]))

(defn two-pairs? [hand]
  (let [handy (sort
                (vals
                  (frequencies
                    (map rank hand))))]
    (or (= handy [1 2 2]) (full-house? hand) (four-of-a-kind? hand))))

(defn straight? [hand]
  (let [ranks (sort (map rank hand))
        min-rank (apply min ranks)]
    (or 
      (= ranks (range min-rank (+ min-rank 5)))
      (= (sort (replace {14 1} ranks)) (range 1 6)))))

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
