(ns p-p-p-pokerface)

(def replacements {\A 14, \K 13, \Q 12, \J 11, \T 10})

(defn high-card? [hand]
  true)
(defn rank [card]
  (let [[rank _ ] card]
    (if (Character/isDigit rank)
      (Integer/valueOf (str rank))
      (replacements rank))))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn pair? [hand]
  (contains? (set (vals (frequencies (map rank hand)))) 2))

(defn three-of-a-kind? [hand]
  (contains? (set (vals (frequencies (map rank hand)))) 3))

(defn four-of-a-kind? [hand]
  (contains? (set (vals (frequencies (map rank hand)))) 4))

(defn flush? [hand]
  (contains? (set (vals (frequencies (map suit hand)))) 5))

(defn full-house? [hand]
  (and (pair? hand) (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  (let [sorted (sort (vals (frequencies (map rank hand))))]
    (or (= [1 2 2] sorted) (four-of-a-kind? hand))))

(defn straight? [hand]
  (let [ace-low (sort(replace {14 1} (map rank hand)))
        ace-high (sort (map rank hand))]
    (or (= ace-low (range (apply min ace-low) (+ 1 (apply max ace-low))))
        (= ace-high (range (apply min ace-high) (+ 1 (apply max ace-high)))))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn value [hand]
  (let [checkers [high-card? pair? two-pairs? three-of-a-kind? straight?
                  flush? full-house? four-of-a-kind? straight-flush?]
        hand-has-value? (fn [i] (if ((get checkers i) hand)
                                  i
                                  0))]
    (apply max (map hand-has-value? (range 9)))))

