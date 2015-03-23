(ns p-p-p-pokerface)

(defn rank [card]
  (let [subs {\T 10
              \J 11
              \Q 12
              \K 13
              \A 14}
        [rank _] card]
    (if (Character/isDigit rank)
      (Integer/valueOf (str rank))
      (subs rank))))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn more-n? [n h]
  (>= (apply max (vals (frequencies (map rank h))))
      n))

(defn pair? [hand]
  (more-n? 2 hand))

(defn three-of-a-kind? [hand]
  (more-n? 3 hand))

(defn four-of-a-kind? [hand]
  (more-n? 4 hand))

(defn flush? [hand]
  (apply = (map suit hand)))

(defn full-house? [hand]
  (= '(2 3)
     (sort (vals (frequencies (map rank hand))))))

(defn two-pairs? [hand]
  (= '(1 2 2)
     (sort (vals (frequencies (map rank hand))))))

(defn straight? [hand]
  (let [ace-high (sort (map rank hand))
        ace-low  (sort (replace {14 1} (map rank hand)))
        ordered? #(= % (range (first %) (last %)))]
    (or (ordered? ace-high)
        (ordered? ace-low))))

(defn straight-flush? [hand]
  nil)

(defn value [hand]
  nil)
