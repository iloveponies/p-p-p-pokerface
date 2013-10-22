(ns p-p-p-pokerface)

(defn rank [card]
  (let [letter-ranks {\T 10, \J 11, \Q 12
                      \K 13, \A 14}
        rank (get card 0)]
    (if (Character/isDigit rank)
      (Integer/valueOf (str rank))
      (get letter-ranks rank))))

(defn suit [card]
  (str (get card 1)))

(defn pair? [hand]
  (contains?(set (vals (frequencies (map rank hand)))) 2))

(defn three-of-a-kind? [hand]
  (contains?(set (vals (frequencies (map rank hand)))) 3))

(defn four-of-a-kind? [hand]
  (contains?(set (vals (frequencies (map rank hand)))) 4))

(defn flush? [hand]
  (contains?(set (vals (frequencies (map suit hand)))) 5))

(defn full-house? [hand]
  (and (pair? hand) (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  (let [ranks (map rank hand)]
  	(or (= (sort (vals (frequencies ranks))) (seq [1 2 2]))
  	    (four-of-a-kind? hand))))

(defn straight? [hand]
  (let [sorted-ranks (vec (sort (map rank hand)))
         contains-ace (contains?(set sorted-ranks) 14)
         sorted-lowace (vec (sort (replace {14 1} sorted-ranks)))]
     (or
      (= (range (get sorted-ranks 0) (+ (get sorted-ranks 4) 1)) sorted-ranks)
      (= (range (get sorted-lowace 0) (+ (get sorted-lowace 4) 1)) sorted-lowace))))
;tän ois voinu tehä niin paljon helpomminkin...

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
