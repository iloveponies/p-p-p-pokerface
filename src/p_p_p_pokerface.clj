(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rnk _] card]
       (if(Character/isDigit rnk)
       (Integer/valueOf (str rnk))
       (get {\T 10, \J 11, \Q 12, \K 13, \A 14} rnk))))

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
  (apply = (map suit hand)))

(defn full-house? [hand]
  (let [sortedfreq (sort (vals (frequencies (sort (map rank hand)))))]
        (= (range 2 4) sortedfreq)))

(defn two-pairs? [hand]
  (let [sortedfreq (vec (sort (vals (frequencies (sort (map rank hand))))))]
    (or (four-of-a-kind? hand)
        (and (<= 3 (count sortedfreq)) (== 2 (get sortedfreq 1) (get sortedfreq 2))))))

; aivan kamalaa koodia ;__;

(defn straight? [hand]
 (let [sorted-ranks (sort (map rank hand))
       min (first sorted-ranks)]

   (defn dimstraight? [sorted-ranks2 min2] ;
     (== 4 (last (map (fn [rank] (- rank min2)) sorted-ranks2))))

   (cond
    (pair? hand) false
    :else (or (dimstraight? sorted-ranks min)
              (dimstraight? (sort (replace {14 1} sorted-ranks)) 1))))) ; replace A


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

