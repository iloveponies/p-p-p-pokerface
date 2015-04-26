(ns p-p-p-pokerface)

(defn rank [card]
  (let [[num _] card s (str num)]
  (cond
   (= s "T") 10
   (= s "J") 11
   (= s "Q") 12
   (= s "K") 13
   (= s "A") 14
   :else (Integer/valueOf s)
   )))

(defn suit [card]
  (let [[_ snd] card]
  (str snd)))

(defn pair? [hand]
  (let [maximum (apply max (vals (frequencies (map rank hand))))]
    (>= maximum 2)))

(defn three-of-a-kind? [hand]
  (let [maximum (apply max (vals (frequencies (map rank hand))))]
    (>= maximum 3)))

(defn four-of-a-kind? [hand]
  (let [maximum (apply max (vals (frequencies (map rank hand))))]
    (>= maximum 4)))

(defn flush? [hand]
  (let [color (frequencies (map suit hand))]
    (== 1 (count color))))

(defn full-house? [hand]
  (let [[a b] (vals (frequencies (map rank hand)))]
    (cond
     (and (== a 2) (== b 3)) true
     (and (== a 3) (== b 2)) true
     :else false
     )))

(defn two-pairs? [hand]
  (let [[a b] (vals (frequencies (map rank hand)))]
    ;(print hand)
    ;(print a " " b)
    (cond
    (and (= a b 2)) true
    (four-of-a-kind? hand) true
    :else false)))

(defn straight? [hand]
  (defn diffs [arr] (map - arr (rest arr)))
  (defn diffsIsOne [arr] (= 1 (count (set (diffs arr)))) )

  (let [nums (sort (map rank hand))
        swapd (sort (replace {14,1} nums))]
  (or (diffsIsOne nums) (diffsIsOne swapd))))


(defn straight-flush? [hand]
  ;(print hand)
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
