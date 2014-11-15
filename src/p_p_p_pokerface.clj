(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rank _] card]
    (if (Character/isDigit rank)
      (Integer/valueOf (str rank))
      (get {\T 10, \J 11, \Q 12, \K 13, \A 14} rank))))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn pair? [hand]
  (= (apply max (vals(frequencies(map rank hand)))) 2))

(defn three-of-a-kind? [hand]
  (= (apply max (vals(frequencies(map rank hand)))) 3))

(defn four-of-a-kind? [hand]
  (= (apply max (vals(frequencies(map rank hand)))) 4))

(defn flush? [hand]
  (= (apply max (vals(frequencies(map suit hand)))) 5))

(defn full-house? [hand]
  (let [full-house (sort(vals(frequencies(map rank hand))))]
    (= [2 3] full-house)))

(defn two-pairs? [hand]
  (let [two-pairs (sort(vals(frequencies(map rank hand))))]
    (or (= [1 2 2] two-pairs) (= [1 4] two-pairs))))

(defn straight? [hand]
  (let [straight (sort(map rank hand))]
    (if (= straight (range (first straight) (+ (first straight) 5)))
      true
      (if (= (last straight) 14)
        (= straight [2 3 4 5 14])
        false))))

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
