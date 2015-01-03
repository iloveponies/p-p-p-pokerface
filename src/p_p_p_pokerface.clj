(ns p-p-p-pokerface)

(defn rank [card]
  (let [[r _] card replacements {\T 10, \J 11, \Q 12, \K 13, \A 14}]
    (if (Character/isDigit r)
      (Integer/valueOf (str r))
      (replacements r))))

(defn suit [card]
  (let [[_ s] card]
    (str s)))

(defn n-of-kinds [n hand]
  (not (empty? (filter #(= n %) (vals (frequencies (map rank hand)))))))

(defn pair? [hand]
  (n-of-kinds 2 hand))

(defn three-of-a-kind? [hand]
  (n-of-kinds 3 hand))

(defn four-of-a-kind? [hand]
  (n-of-kinds 4 hand))

(defn flush? [hand]
  (= 5 (first (vals (frequencies (map suit hand))))))

(defn full-house? [hand]
  (and (three-of-a-kind? hand) (pair? hand)))

(defn two-pairs? [hand]
  (= 2 (count (filter #(= 2 %) (vals (frequencies (map rank hand)))))))

(defn low-vals [vals]
  (apply min vals))

(defn high-vals [vals]
  (apply max vals))

(defn straight? [hand]
  (let [sorted-vals (sort compare (map rank hand))
        min-val (low-vals sorted-vals)
        high-val (high-vals sorted-vals)
        range-val (range min-val (inc high-val))]
    (cond
      (= [1 2 3 4 5] (sort compare (replace {14 1} sorted-vals))) true
      :else (and (= range-val sorted-vals) (= 5 (count range-val))))))

(defn straight-flush? [hand]
  (and (flush? hand) (straight? hand)))

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