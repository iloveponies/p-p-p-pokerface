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
    (if(= x y)
      true
      (= (sort(replace {14 1} x)) (range 1 6)))))

(defn flush? [hand]
  (= 5 (apply max(vals(frequencies(map suit hand))))))

(defn full-house? [hand]
  (let [[x y] (sort(vals(frequencies(map rank hand))))]
    (and (== 2 x) (== 3 y ))))

(defn straight-flush? [hand]
  (and (flush? hand) (straight? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let[checkers [high-card? pair?
           		  two-pairs? three-of-a-kind?
        		  straight? flush?
           		  full-house? four-of-a-kind?
                  straight-flush?]
       hand-has-value? (fn [hand n] ((get checkers n) hand))
       tr (fn [x](hand-has-value? hand x))]
    (apply max (filter tr (range 0 9)))))