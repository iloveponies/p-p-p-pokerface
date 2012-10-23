(ns p-p-p-pokerface)

(defn suit [[_ s]]
  (str s))

(defn rank [card]
  (let[[s _] card
       r {\T 10, \J 11, \Q 12, \K 13, \A 14}]
    (if(contains? r s)
      (get r s)
      (Integer/valueOf (str s)))))

(defn pair? [hand]
  (< 1 (apply max (vals(frequencies(map rank hand))))))

(defn two-pairs? [hand]
  (let [[x y] (reverse(sort(vals(frequencies(map rank hand)))))]
    (or (and (<= 2 x) (<= 2 y)) (<= 4 x))))

(defn three-of-a-kind? [hand]
  (< 2 (apply max (vals(frequencies(map rank hand))))))

(defn four-of-a-kind? [hand]
  (< 3 (apply max (vals(frequencies(map rank hand))))))

(defn straight? [hand]
  (let [x (sort(map rank hand))
        y (range (first x)(+ 1 (first(reverse x))))]
	(reverse x)
    (if(== 0 (compare (apply str x) (apply str y)))
      true
      (if(contains? x "14")
        ((sort(replace {"14" "1"} x))
         (== 0 (compare (sort x) (range (first x)(+ 1 (first(reverse x)))))))
        false))))

(defn flush? [hand]
  (= 5 (apply max(vals(frequencies(map suit hand))))))

(defn full-house? [hand]
  (let [[x y] (sort(vals(frequencies(map rank hand))))]
    (and (== 2 x) (== 3 y ))))

(defn straight-flush? [hand]
  (and (flush? hand) (straight? hand)))

(defn value [hand]
  nil)