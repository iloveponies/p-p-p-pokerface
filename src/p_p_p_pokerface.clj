(ns p-p-p-pokerface)

(defn rank [card]
  (let [x (get card 0)]
   (cond (Character/isDigit x)
         (Integer/valueOf (str x))
         :else
         (get {\T 10 \J 11 \Q 12 \K 13 \A 14} x))))

(defn suit [card]
  (str (get card 1)))

(defn pair? [hand]
  (< 1 (apply max (vals (frequencies (map rank hand))))))

(defn three-of-a-kind? [hand]
  (< 2 (apply max (vals (frequencies (map rank hand))))))

(defn four-of-a-kind? [hand]
  (< 3 (apply max (vals (frequencies (map rank hand))))))

(defn flush? [hand]
  (= 5 (apply max (vals (frequencies (map suit hand))))))

(defn full-house? [hand]
  (and (= 3 (apply max (vals (frequencies (map rank hand)))))
  (= 2 (apply min (vals (frequencies (map rank hand)))))))

(defn two-pairs? [hand]
  (or (= [1 2 2] (sort (vals (frequencies (map rank hand)))))
      (full-house? hand) (four-of-a-kind? hand)))

(defn straight? [hand]
  (or
  (let [x (map rank hand)
        y (vals (frequencies x))]
   (and (= (- (apply max x) (apply min x)) 4)
        (= (apply max y) 1)))
  (let [x (replace {14 1} (map rank hand))
        y (vals (frequencies x))]
   (and (= (- (apply max x) (apply min x)) 4)
        (= (apply max y) 1)))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

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
