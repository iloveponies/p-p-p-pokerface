(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rank-ch _] card]
    (if (Character/isDigit rank-ch)
      (Integer/valueOf (str rank-ch))
      (let [rank-ch->rank-num {\T 10, \J 11, \Q 12, \K 13, \A 14}]
        (rank-ch->rank-num rank-ch)))))

(defn suit [card]
  (let [[_ suit-ch] card]
    (str suit-ch)))

(defn pair? [hand]
  (<= 2 (apply max (vals (frequencies (map rank hand))))))

(defn three-of-a-kind? [hand]
  (<= 3 (apply max (vals (frequencies (map rank hand))))))

(defn four-of-a-kind? [hand]
  (<= 4 (apply max (vals (frequencies (map rank hand))))))

(defn flush? [hand]
  (<= 5 (apply min (vals (frequencies (map suit hand))))))

(defn full-house? [hand]
  (let [ranks-freq (vals (frequencies (map rank hand)))]
    (and
     (= 3 (apply max ranks-freq))
     (= 2 (apply min ranks-freq)))))

(defn two-pairs? [hand]
  (or (= 2 (get (frequencies (vals (frequencies (map rank hand)))) 2))
      (four-of-a-kind? hand)))

(defn straight? [hand]
  (let [sorted-ranks (sort (map rank hand))
        first-rank (first sorted-ranks)
        template (if (and (some #{2} sorted-ranks) (some #{14} sorted-ranks))
                   (seq '(2 3 4 5 14))
                   (range first-rank (+ first-rank 5)))]
    (= sorted-ranks template)))

(defn straight-flush? [hand]
  (and (straight? hand)
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
