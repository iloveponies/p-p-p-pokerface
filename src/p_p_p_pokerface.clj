(ns p-p-p-pokerface)

(defn rank [card]
  (let [[fst] card]
  (cond
     (Character/isDigit fst) (Integer/valueOf (str fst))
     (= (str fst) "T") 10
     (= (str fst) "J") 11
     (= (str fst) "Q") 12
     (= (str fst) "K") 13
     (= (str fst) "A") 14
  )))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn pair? [hand]
  (= 1 (count
     (filter (fn[x] (= x 2)) (vals (frequencies (map rank hand)))))))

(defn three-of-a-kind? [hand]
  (= 1 (count
   (filter (fn[x] (= x 3)) (vals (frequencies (map rank hand)))))))

(defn four-of-a-kind? [hand]
  (= 1 (count
   (filter (fn[x] (= x 4)) (vals (frequencies (map rank hand)))))))

(defn flush? [hand]
  (= 1 (count
   (filter (fn[x] (= x 5)) (vals (frequencies (map suit hand)))))))

(defn full-house? [hand]
  (and (= (second (sort (vals (frequencies (map rank hand)))))
          3)
       (= (first (sort (vals (frequencies (map rank hand)))))
          2)))

(defn two-pairs? [hand]
  (= 2 (count
     (filter (fn[x] (= x 2)) (vals (frequencies (map rank hand)))))))

(defn straight? [hand]
  (or (= (sort (map rank hand))
         (range (apply min (map rank hand))
                (+ (apply min (map rank hand)) 5)))
      (= (sort (replace {14 1} (map rank hand)))
         (range (apply min (replace {14 1} (map rank hand)))
                (+ (apply min (replace {14 1} (map rank hand))) 5)))))

(defn straight-flush? [hand]
  (and (flush? hand)
       (straight? hand)))

(defn high-card? [hand]
  true)

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
   (high-card? hand) 0
  ))
