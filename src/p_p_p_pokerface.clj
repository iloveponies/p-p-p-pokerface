(ns p-p-p-pokerface)

(defn rank [[r _]]
  (if (Character/isDigit r)
    (Integer/valueOf (str r))
    (get {\T 10, \J 11, \Q 12, \K 13, \A 14} r)))

(defn suit [[_ s]]
  (str s))

(defn pair? [hand]
  (boolean (first (filter (fn [x] (< 1 x)) (vals (frequencies (map rank hand)))))))

(defn three-of-a-kind? [hand]
  (boolean (first (filter (fn [x] (< 2 x)) (vals (frequencies (map rank hand)))))))

(defn four-of-a-kind? [hand]
  (boolean (first (filter (fn [x] (< 3 x)) (vals (frequencies (map rank hand)))))))

(defn flush? [hand]
  (apply = (map suit hand)))

(defn full-house? [hand]
  (= [2 3] (sort (vals (frequencies (map rank hand))))))

(defn two-pairs? [hand]
  (= [1 2 2] (sort (vals (frequencies (map rank hand))))))

(defn straight? [hand]
  (let [sorted (sort (map rank hand))]
    (or
      (= (range (first sorted) (+ 5 (first sorted))) sorted)
      (= [2 3 4 5 14] sorted))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn value [hand]
  (let [xs [[pair? 1]
            [two-pairs? 2]
            [three-of-a-kind? 3]
            [straight? 4]
            [flush? 5]
            [full-house? 6]
            [four-of-a-kind? 7]
            [straight-flush? 8]
            [(fn [_] true) 0]]
        match (fn [[f _]] (f hand))]
    (apply max (map second (filter match xs)))))
