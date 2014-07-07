(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rank suit] card]
    (if (Character/isDigit rank)
      (Integer/valueOf (str rank))
      (get {\T 10
            \J 11
            \Q 12
            \K 13
            \A 14}
           rank))))

(defn suit [card]
  (let [[rank suit] card]
    (str suit)))

(defn pair? [hand]
  (> (apply max
            (vals (frequencies (map rank
                                    hand))))
     1))

(defn three-of-a-kind? [hand]
  (> (apply max
            (vals (frequencies (map rank
                                    hand))))
     2))

(defn four-of-a-kind? [hand]
  (> (apply max
            (vals (frequencies (map rank
                                    hand))))
     3))

(defn flush? [hand]
  (= (count (frequencies (map suit
                              hand)))
     1))

(defn full-house? [hand]
  (= [2 3]
     (sort (vals (frequencies (map rank
                                   hand))))))

(defn two-pairs? [hand]
  (or (four-of-a-kind? hand)
      (= [1 2 2]
         (sort (vals (frequencies (map rank
                                   hand)))))))

(defn straight? [hand]
  (let [sorted-ranks (sort (map rank
                                hand))]
    (or (= sorted-ranks [2 3 4 5 14])
        (and (= (count (frequencies sorted-ranks))
                5)
             (= 4
                (- (last sorted-ranks)
                   (first sorted-ranks)))))))

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
