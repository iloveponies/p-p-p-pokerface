(ns p-p-p-pokerface)

(defn rank [card]
  (let [[fst _] card replacements {\T 10, \J 11, \Q 12, \K 13, \A 14}]
    (if (Character/isDigit fst)
      (Integer/valueOf (str fst))
      (replacements fst)
      )))

(defn suit [card]
  (let [[_ scn] card]
    (str scn)))

(defn pair? [hand]
  (>= (apply max (vals (frequencies (map rank hand)))) 2))

(defn three-of-a-kind? [hand]
  (>= (apply max (vals (frequencies (map rank hand)))) 3))

(defn four-of-a-kind? [hand]
  (>= (apply max (vals (frequencies (map rank hand)))) 4))


(defn flush? [hand]
  (= (apply max (vals (frequencies (map suit hand)))) 5))

(defn full-house? [hand]
  (= (seq [2 3]) (sort (vals (frequencies (map rank hand))))))

(defn two-pairs? [hand]
  (let [sorted-values ( sort( vals( frequencies(map rank hand))))]
   (or (= (seq [1 2 2]) sorted-values) (four-of-a-kind? hand ))))

(defn straight? [hand]
  (let [sorted-rank-high-ace (sort(map rank hand))
        sorted-rank-low-ace (sort (replace {14 1} (map rank hand)))
        check-function (fn [sorted-rank] (= (range (first sorted-rank) (+ (last sorted-rank) 1)) sorted-rank))]
    (or (check-function sorted-rank-high-ace) (check-function sorted-rank-low-ace))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))


(defn value [hand]
  (let [checkers #{[pair? 1] [two-pairs? 2]
                   [three-of-a-kind? 3] [straight? 4]
                   [flush? 5] [full-house? 6]
                   [four-of-a-kind? 7] [straight-flush? 8]} ]
   (apply max (map
     (fn [checker] (if ((first checker) hand)
                     (second checker)
                     0) )
        checkers))))







