(ns p-p-p-pokerface)

(defn rank [[r _]]
  (let [rep {\T 10 \J 11 \Q 12 \K 13 \A 14}]
    (if (Character/isDigit r)
      (Integer/valueOf (str r))
      (rep r))))

(defn suit [[_ s]]
  (str s))

(defn pair? [hand]
  (boolean (some (fn [x] (>= x 2)) (vals (frequencies (map rank hand))))))

(defn three-of-a-kind? [hand]
  (boolean (some (fn [x] (>= x 3)) (vals (frequencies (map rank hand))))))

(defn four-of-a-kind? [hand]
  (boolean (some (fn [x] (>= x 4)) (vals (frequencies (map rank hand))))))

(defn flush? [hand]
  (boolean (some (fn [x] (>= x 5)) (vals (frequencies (map suit hand))))))

(defn full-house? [hand]
  (= [2 3] (sort (vals (frequencies (map rank hand))))))

(defn two-pairs? [hand]
  (= [1 2 2] (sort (vals (frequencies (map rank hand))))))

(defn straight? [hand]
  (let [h1 (sort (map rank hand))
        h2 (sort (replace {14 1} h1))
        r1 (range (first h1) (+ (first h1) 5))
        r2 (range (first h2) (+ (first h2) 5))]
    (or (= h1 r1) (= h2 r2))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn value [hand]
  (cond (straight-flush? hand) 8
        (four-of-a-kind? hand) 7
        (full-house? hand) 6
        (flush? hand) 5
        (straight? hand) 4
        (three-of-a-kind? hand) 3
        (two-pairs? hand) 2
        (pair? hand) 1
        :else 0))
