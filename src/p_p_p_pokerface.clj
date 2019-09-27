(ns p-p-p-pokerface)

(defn rank [card]
  (let[[value _] card]
    (if(Character/isDigit value)
      (Integer/valueOf (str value))
      (get {\A 14, \K 13, \Q 12, \J 11, \T 10} value )  )))

(defn suit [card]
  (str(let [[_ suit] card]
  suit)))

(defn pair? [hand]
   (<= 2 (apply max(vals(frequencies(map rank hand))))) )

(defn three-of-a-kind? [hand]
  (<= 3 (apply max(vals(frequencies(map rank hand))))))

(defn four-of-a-kind? [hand]
  (<= 4 (apply max(vals(frequencies(map rank hand))))))

(defn flush? [hand]
  (<= 5 (apply max(vals(frequencies(map suit hand))))))

(defn full-house? [hand]
  (= [2 3] (sort(vals(frequencies(map rank hand))))))

(defn two-pairs? [hand]
  (or (= [1 2 2] (sort(vals(frequencies(map rank hand))))) (four-of-a-kind? hand) ))

(defn straight? [hand]
  (let [sorted (sort(map rank hand))
        replaced (sort(replace {14 1} sorted))]
    (or (= (range (apply min sorted) (+ 1 (apply max sorted)))sorted)
        (= (range (apply min replaced) (+ 1 (apply max replaced)))replaced))))

(defn straight-flush? [hand]
  (and (flush? hand) (straight? hand) ))

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
