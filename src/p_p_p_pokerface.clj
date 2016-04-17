(ns p-p-p-pokerface)

(def ranks {\T 10 \J 11 \Q 12 \K 13 \A 14})

(defn rank [[r _]]
  (if (Character/isDigit r)
    (Integer/valueOf (str r))
    (get ranks r)))

(defn suit [[_ s]]
  (str s))

(defn frequencies-by [f xs]
  (frequencies (map f xs)))

(defn by-rank [hand]
  (frequencies-by rank hand))

(defn by-suit [hand]
  (frequencies-by suit hand))

(defn max-frequency [xs]
  (apply max (vals xs)))

(defn n-of-a-rank? [n hand]
  (= n (max-frequency (by-rank hand))))

(defn n-of-a-suit? [n hand]
  (= n (max-frequency (by-suit hand))))

(def pair? (partial n-of-a-rank? 2))

(def three-of-a-kind? (partial n-of-a-rank? 3))

(def four-of-a-kind? (partial n-of-a-rank? 4))

(defn flush? [hand]
  (n-of-a-suit? (count hand) hand))

(defn full-house? [hand]
  (= [2 3]
     (sort (vals (by-rank hand)))))

(defn two-pairs? [hand]
  (= 2
     (get (frequencies (vals (by-rank hand))) 2)))

(defn incremental? [xs]
  (apply = 1 (map - (rest xs) xs)))

(defn straight? [hand]
  (let [xs (sort (map rank hand))
        ys (sort (replace {14 1} xs))]
    (or
      (incremental? xs)
      (incremental? ys))))

(defn straight-flush? [hand]
  (and (straight? hand)
       (flush? hand)))

(def checkers #{[pair? 1]       [straight-flush? 8]
                [two-pairs? 2]  [three-of-a-kind? 3]
                [straight? 4]   [flush? 5]
                [full-house? 6] [four-of-a-kind? 7]})

(defn value [hand]
  (let [check (fn [[f v]] (if (f hand) v 0))]
    (apply max (map check checkers))))
