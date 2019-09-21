(ns p-p-p-pokerface)

(defn rank [card]
  (let [x {\A 14, \K 13, \Q 12, \J 11, \T 10}]
   (let [[f _] card]
    (if (Character/isDigit f)
      (Integer/valueOf (str f))
      (get x f)))))

(defn suit [card]
  (let [[_ s] card]
    (str s)))

(defn pair? [hand]
  (let [x (map rank hand)]
    (if (== (apply max(vals(frequencies x))) 2)
      true
      false)))

(defn three-of-a-kind? [hand]
  (let [x (map rank hand)]
    (if (== (apply max(vals(frequencies x))) 3)
      true
      false)))

(defn four-of-a-kind? [hand]
  (let [x (map rank hand)]
    (if (== (apply max(vals(frequencies x))) 4)
      true
      false)))

(defn flush? [hand]
  (let [x (map suit hand)]
    (if (== (apply max(vals(frequencies x))) 5)
      true
      false)))

(defn full-house? [hand]
  (let [x (sort(map rank hand))]
    (if (= [2 3] (sort (vals (frequencies x))))
      true
      false)))

(defn two-pairs? [hand]
  (let [x (sort(map rank hand))]
    (if (= [1 2 2] (sort (vals (frequencies x))))
      true
      false)))

(defn straight? [hand]
  (let [x (sort(map rank hand))]
    (cond
     (= x [2 3 4 5 14]) true
     (and (== (apply max(vals(frequencies x))) 1)(= (-(nth x 4)(nth x 0)) 4)) true
     :else false)))

(defn straight-flush? [hand]
  (and (= (straight? hand) true)(= (flush? hand) true)))

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
