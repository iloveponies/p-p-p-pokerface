(ns p-p-p-pokerface)

(defn rank [card]
  (let [rep {\T 10, \J 11, \Q 12, \K 13, \A 14}
        [f s] card]
    (if (Character/isDigit f)
      (Integer/valueOf (str f))
      (rep f))))

(defn suit [card]
  (let [[fir sec] card]
    (str sec)))

(defn pair? [hand]
  (let [v (vals (frequencies (map rank hand)))]
    (contains? (set v) 2)
      ))

(defn three-of-a-kind? [hand]
  (let [v (vals (frequencies (map rank hand)))]
    (contains? (set v) 3)
      ))

(defn four-of-a-kind? [hand]
  (let [v (vals (frequencies (map rank hand)))]
    (contains? (set v) 4)
      ))

(defn flush? [hand]
  (let [v (vals (frequencies (map suit hand)))]
    (contains? (set v) 5)
      ))

(defn full-house? [hand]
  (and
    (three-of-a-kind? hand)
    (pair? hand)))

(defn two-pairs? [hand]
  (let [v (sort (vals (frequencies (map rank hand))))]
       (or
         (= v [1 4])
         (= v [1 2 2]))
      )
  )

(defn straight? [hand]
  (let [v (sort (map rank hand))
        min-val (apply min v)
        max-val (+ (apply max v) 1)]
    (or
      (= (range min-val max-val) v)
      (= (range 1 6) (sort (replace {14 1} v)))
    )
    ))

(defn straight-flush? [hand]
  (and
    (straight? hand)
    (flush? hand)))


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
