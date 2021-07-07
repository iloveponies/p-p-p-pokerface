(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rank _] card
        replacements {\T 10, \J 11, \Q 12, \K 13, \A 14}]
    (if (Character/isDigit rank)
      (Integer/valueOf (str rank))
      (get replacements rank))))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn pair? [hand]
  (> (apply max (vals (frequencies (map rank hand)))) 1))

(defn three-of-a-kind? [hand]
  (> (apply max (vals (frequencies (map rank hand)))) 2))

(defn four-of-a-kind? [hand]
  (> (apply max (vals (frequencies (map rank hand)))) 3))

(defn flush? [hand]
  (apply = (map suit hand)))

(defn full-house? [hand]
  (let [sorted-ranks (vec (sort (vals (frequencies (map rank hand)))))]
    (and (== (get sorted-ranks 0) 2)
         (== (get sorted-ranks 1) 3))))

(defn two-pairs? [hand]
  (let [sorted-ranks (vec (sort (vals (frequencies (map rank hand)))))]
    (or (four-of-a-kind? hand)
        (full-house? hand)
        (and (== (get sorted-ranks 0) 1)
             (== (get sorted-ranks 1) 2)
             (== (get sorted-ranks 2) 2)))))

(defn straight? [hand]
  (let [sorted-ranks (vec (sort (map rank hand)))
        first-rank (get sorted-ranks 0)]
    (cond
      (= sorted-ranks (range first-rank (+ first-rank 5))) true
      :else (= (vec (sort (replace {14 1} sorted-ranks))) (range 1 6)))))

(defn straight-flush? [hand]
  (and (flush? hand) (straight? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers [[high-card? 0]  [pair? 1]
                   [two-pairs? 2]  [three-of-a-kind? 3]
                   [straight? 4]   [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]]]
    (apply max
           (map second
                (filter
                  (fn [custom-function]
                    ((first custom-function) hand))
                  checkers)))))
