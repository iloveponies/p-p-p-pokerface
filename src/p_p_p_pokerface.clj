(ns p-p-p-pokerface)

(defn rank [card]
  (let [[value] card]
    (if (Character/isDigit value)
      (Integer/valueOf (str value))
      (get {\T 10 \J 11 \Q 12 \K 13 \A 14} value))))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn pair? [hand]
  (let [values (map rank hand)]
    (contains? (set (vals (frequencies values))) 2)))

(defn three-of-a-kind? [hand]
  (let [values (map rank hand)]
    (contains? (set (vals (frequencies values))) 3)))

(defn four-of-a-kind? [hand]
  (let [values (map rank hand)]
    (contains? (set (vals (frequencies values))) 4)))

(defn flush? [hand]
  (let [suits (map suit hand)]
    (contains? (set (vals (frequencies suits))) 5)))

(defn full-house? [hand]
  (and (pair? hand) (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  (let [values (sort (vals (frequencies (map rank hand))))]
    (or (= values [1 2 2])
        (= values [1 4]))))

(defn straight? [hand]
  (let [sorted-values (sort (map rank hand))
        sorted-values2 (sort (replace {14 1} sorted-values))
        [fst-value] sorted-values]
     (or (= sorted-values (range fst-value (+ fst-value 5)))
         (= sorted-values2 (range 1 (+ 1 5))))))


(defn straight-flush? [hand]
  (and (flush? hand) (straight? hand)))

(defn high-card? [hand]
  true)

(defn hand-has-value? [hand value]
  (let [checkers [high-card? pair? two-pairs? three-of-a-kind? straight?
                  flush? full-house? four-of-a-kind? straight-flush?]]
   ((get checkers value) hand)))

(defn value [hand]
  (cond
   (hand-has-value? hand 8) 8
   (hand-has-value? hand 7) 7
   (hand-has-value? hand 6) 6
   (hand-has-value? hand 5) 5
   (hand-has-value? hand 4) 4
   (hand-has-value? hand 3) 3
   (hand-has-value? hand 2) 2
   (hand-has-value? hand 1) 1
   (hand-has-value? hand 0) 0))
