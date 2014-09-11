(ns p-p-p-pokerface)

(defn rank [card]
  (let [[a _] card]
    (cond
     (= \T a) 10
     (= \J a) 11
     (= \Q a) 12
     (= \K a) 13
     (= \A a) 14
     :else (Integer/valueOf (str a))
     )))

(defn suit [card]
  (str (let [[_ a] card] a)))



(defn n-of-a-kind? [hand n]
  (contains?
   (set (vals (frequencies (map rank hand)))) n))

(defn pair? [hand]
  (n-of-a-kind? hand 2))

(defn three-of-a-kind? [hand]
  (n-of-a-kind? hand 3))

(defn four-of-a-kind? [hand]
  (n-of-a-kind? hand 4))

(defn flush? [hand]
  (= (vals (frequencies (map suit hand))) (seq [5])))

(defn full-house? [hand]
  (= (vals (frequencies (map rank hand)))
     (seq [3 2])))

(defn two-pairs? [hand]
  (= (vals (frequencies (map rank hand)))
     (seq [2 2 1])))

(defn straight? [hand]
  (let [v (sort (map rank hand))
        m (apply min v)]
    (if (= (range m (+ m 5)) v) true
      (and (= 2 m) (= (apply max v) 14)))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

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
