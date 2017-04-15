(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rank _] card
        values {\T 10, \J 11, \Q 12, \K 13, \A 14}]
    (if (Character/isDigit rank)
      (Integer/valueOf (str rank))
      (get values rank))))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn pair? [hand]
  (== (apply max (vals (frequencies (map rank hand)))) 2))

(defn three-of-a-kind? [hand]
  (== (apply max (vals (frequencies (map rank hand)))) 3))

(defn four-of-a-kind? [hand]
  (== (apply max (vals (frequencies (map rank hand)))) 4))

(defn flush? [hand]
  (== (first (vals (frequencies (map suit hand)))) 5))

(defn full-house? [hand]
  (let [srtdhnd (sort (vals (frequencies (map rank hand))))]
    (and
     (== (first srtdhnd) 2)
     (== (second srtdhnd) 3))))

(defn two-pairs? [hand]
  (let [srtdhnd (sort (vals (frequencies (map rank hand))))]
    (== (count (filter
                (fn [x] (== x 2))
                srtdhnd))
        2)))

(defn straight? [hand]
  (let [conv {14 1}
        tagrnks (vec (map rank hand))
        ranks (if (== 2 (apply min tagrnks))
                (sort (replace conv tagrnks))
                (sort tagrnks))]
    (= ranks
       (range (apply min ranks)
              (+ (apply min ranks) 5)))))

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
