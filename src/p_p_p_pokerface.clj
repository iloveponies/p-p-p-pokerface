(ns p-p-p-pokerface)

(def replacements {\T 10, \J 11, \Q 12, \K 13, \A 14})

(defn rank [card]
  (let [[rank _] card]
    (if (Character/isDigit rank)
    (Integer/valueOf (str rank))
    (replacements rank))))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn pair? [hand]
  (<= 2 (apply max (vals (frequencies (map rank hand))))))

(defn three-of-a-kind? [hand]
  (<= 3 (apply max (vals (frequencies (map rank hand))))))

(defn four-of-a-kind? [hand]
  (<= 4 (apply max (vals (frequencies (map rank hand))))))

(defn flush? [hand]
  (apply = (map suit hand)))

(defn full-house? [hand]
  (let [[f s] (sort (vals (frequencies (map rank hand))))]
    (and (== 2 f) (== 3 s))))

(defn two-pairs? [hand]
  (let [[f s t] (sort (vals (frequencies (map rank hand))))]
    (or (four-of-a-kind? hand) (== 2 s t))))

(defn straight? [hand]
  (let [v (sort (map rank hand))
        f (first v)
        l (nth v 4)]
    (if (== l 14)
      (or (= v (range 10 15)) (= v [2 3 4 5 14]))
      (= v (range f (+ l 1))))))

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
