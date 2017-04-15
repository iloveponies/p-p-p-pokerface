(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rank _] card]

  (def ranks {\A 14, \K 13, \Q 12, \J 11, \T 10})

  (cond
    (Character/isDigit rank) (Integer/valueOf (str rank))
    :else (Integer/valueOf (str (ranks rank))))))

(defn suit [card]
  (let[[_ suit] card]
  (str suit)))

(defn pair? [hand]
  (= 2 (apply max(vals (frequencies (map rank hand))))))

(defn three-of-a-kind? [hand]
  (= 3 (apply max(vals (frequencies (map rank hand))))))

(defn four-of-a-kind? [hand]
  (= 4 (apply max(vals (frequencies (map rank hand))))))

(defn flush? [hand]
  (= 5 (apply max(vals (frequencies (map suit hand))))))

(defn full-house? [hand]
  (= (seq [2 3]) (sort (vals (frequencies (map rank hand))))))

(defn two-pairs? [hand]
  (= (seq [1 2 2]) (sort (vals (frequencies (map rank hand))))))

(defn straight? [hand]
  (let [sorted1 (sort (map rank hand))
        sorted1-cmp (range (apply min sorted1)(+ 5 (apply min sorted1)))
        sorted2 (sort (replace {14 1} sorted1))
        sorted2-cmp (range (apply min sorted2)(+ 5 (apply min sorted2)))]
    (or
      (and
        (= sorted1 sorted1-cmp))
      (and
        (apply not= sorted2)
        (= sorted2 sorted2-cmp)))))


(defn straight-flush? [hand]
  (and (straight? hand)(flush? hand)))

(defn value [hand]
  (cond
     (straight-flush? hand)  8
     (four-of-a-kind? hand)  7
     (full-house? hand)      6
     (flush? hand)           5
     (straight? hand)        4
     (three-of-a-kind? hand) 3
     (two-pairs? hand)       2
     (pair? hand)            1
     :else                   0))
