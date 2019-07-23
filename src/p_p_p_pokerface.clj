(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rank _] card]
    (if (Character/isDigit rank) (Integer/valueOf (str rank))
        (get {\A 14, \K 13, \Q 12, \J 11, \T 10} rank))))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn- ranks [hand]
  (map rank hand))

(defn pair? [hand]
  (= 2 (apply max (vals (frequencies (ranks hand))))))

(defn three-of-a-kind? [hand]
  (= 3 (apply max (vals (frequencies (ranks hand))))))

(defn four-of-a-kind? [hand]
  (= 4
     (apply max (vals (frequencies (ranks hand))))))

(defn flush? [hand]
  (apply = (map suit hand)))

(defn full-house? [hand]
  (=
   (sort (vals (frequencies (ranks hand))))
   (seq [2 3])))

(defn two-pairs? [hand]
  (=
   (sort (vals (frequencies (map rank hand))))
   (seq [1 2 2])))

(defn straight? [hand]
  (let [sorted (sort (ranks hand))
        minval (apply min (ranks hand))]
    (or
     (= (seq [2 3 4 5 14])
        sorted)
     (= (range minval (+ minval 5))
        sorted))))

(defn straight-flush? [hand]
  (and (straight? hand)
       (flush? hand)))

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
