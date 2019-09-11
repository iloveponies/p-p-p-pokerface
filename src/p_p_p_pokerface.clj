(ns p-p-p-pokerface)

(defn rank [card]
  (let [r {\T 10, \J 11, \Q 12, \K 13, \A 14}
        [f _] card]
      (if (Character/isDigit f) (Integer/valueOf (str f)) (Integer/valueOf (str (r f))))))

(defn suit [card]
  (let [[_ s] card]
    (str s)))

(defn pair? [hand]
  (< 1 (apply max(vals (frequencies (map rank hand))))))


(defn three-of-a-kind? [hand]
  (< 2 (apply max(vals (frequencies (map rank hand))))))

(defn four-of-a-kind? [hand]
  (< 3 (apply max(vals (frequencies (map rank hand))))))

(defn flush? [hand]
  (< 4 (apply max(vals (frequencies (map suit hand))))))

(defn full-house? [hand]
  (= (seq [2 3]) (sort (vals (frequencies (map rank hand))))))

(defn two-pairs? [hand]
  (= (seq [1 2 2]) (sort (vals (frequencies (map rank hand))))))

(defn straight? [hand]
  (let [n (sort (map rank hand))
        f (first n)
        v (map (fn [x] (- x f)) n)]
  (or (= (seq [1 2 3 4 5]) v ) (= (seq [0 1 2 3 4]) v )(= (seq [2 3 4 5 14]) n) (= (seq [10 11 12 13 14]) n))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn value [hand]
  nil)
