(ns p-p-p-pokerface)

(defn rank [card]
  (let [[ch _]  card,
        rnum    (- (int ch) (int \0))]
    (cond
      (< rnum 10)   rnum
      (= (str ch) "T") 10
      (= (str ch) "J") 11
      (= (str ch) "Q") 12
      (= (str ch) "K") 13
      (= (str ch) "A") 14)))

(defn suit [card]
  (let [[_ ch] card]
    (str ch)))

(defn contains-N? [hand N]
  (->
   (vals (frequencies (map rank hand)))
   (set)
   (contains? N)))

(defn pair? [hand]
  (contains-N? hand 2))

(defn three-of-a-kind? [hand]
  (contains-N? hand 3))

(defn four-of-a-kind? [hand]
  (contains-N? hand 4))

(defn flush? [hand]
  (->
   (map suit hand)
   (set)
   (count)
   (= 1)))

(defn full-house? [hand]
  (->
   (vals (frequencies (map rank hand)))
   (set)
   (= #{2 3})))

(defn two-pairs? [hand]
  (->
   (vals (frequencies (map rank hand)))
   (sort)
   (= [1 2 2])))

(defn straight? [hand]
  (let [values (sort (map rank hand))
        min-card (first values)

        values2
        (sort (replace {14 1} values))
        min-card2 (first values2)]
    (or
     (=
      values
      (range min-card (+ min-card 5)))
     (=
      values2
      (range min-card2 (+ min-card2 5))))))

(defn straight-flush? [hand]
  (and (flush? hand) (straight? hand)))

(defn value [hand]
  (cond
   (two-pairs? hand)          2
   (full-house? hand)         6
   (pair? hand)               1
   (three-of-a-kind? hand)    3
   (straight-flush? hand)     8
   (straight? hand)           4
   (flush? hand)              5
   (four-of-a-kind? hand)     7
   :else                      0))
