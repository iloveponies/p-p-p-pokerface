(ns p-p-p-pokerface)


(defn rank [card]
  (let [[rank _] card
        letter-ranks {\T 10, \J 11, \Q 12, \K 13, \A 14}]
    (if (Character/isDigit rank)
      (Integer/valueOf (str rank))
      (letter-ranks rank)
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
  (== 1 (count (frequencies (map suit hand)))))

(defn full-house? [hand]
   (let [how-many-of-a-rank (set(vals (frequencies (map rank hand))))]
     (and (contains? how-many-of-a-rank 2)
          (contains? how-many-of-a-rank 3))))

(defn two-pairs? [hand]
  (let [how-many-of-a-rank (frequencies (map rank hand))]
    (and (== 2 (apply max(vals how-many-of-a-rank)))
         (== 3 (count how-many-of-a-rank)))))




(defn straight? [hand]
  (let [high-ranks (keys (frequencies (map rank hand)))
        low-ranks (replace {14 1} high-ranks)]

    (let [is-straight? (fn [ranks]
                         (and (== 5 (count ranks))
                              (< (apply min ranks)
                                 (apply max ranks)
                                 (+ (apply min ranks) 5))))]
      (or (is-straight? high-ranks)
          (is-straight? low-ranks)))))


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


