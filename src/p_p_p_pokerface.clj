(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rank _] card]
    (def replacements {\T 10, \J 11, \Q 12, \K 13, \A 14})
      (cond (Character/isDigit rank) (Integer/valueOf (str rank))
            :else (replacements rank)

            )))

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
  (<= 5 (apply max (vals (frequencies (map suit hand))))))

(defn full-house? [hand]
  (= [2 3] (sort (vals (frequencies (map rank hand))))))

(defn two-pairs? [hand]
  (= [1 2 2] (sort (vals (frequencies (map rank hand))))))

(defn straight? [hand]
  (or
   (= (sort (map rank hand)) (range (first (min (map rank hand))) (+ (first (min (map rank hand))) 5)))

   (= (sort (replace {14 1} (map rank hand))) (range 1 6))))

(defn straight-flush? [hand]
  (if (and (straight? hand) (flush? hand))
    true
    false))


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

