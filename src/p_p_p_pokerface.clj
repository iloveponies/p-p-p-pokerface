(ns p-p-p-pokerface)

(defn rank [card]
  (let [[num suit] card]
    (cond
      (Character/isDigit num) (Integer/valueOf (str num))
      (= \T num) 10
      (= \J num) 11
      (= \Q num) 12
      (= \K num) 13
      (= \A num) 14)))

(defn suit [card]
  (let [[num suit] card]
    (str suit)))

(defn pair? [hand]
  (= 2 (apply max (vals (frequencies (map rank hand))))))


(defn three-of-a-kind? [hand]
  (= 3 (apply max (vals (frequencies (map rank hand))))))

(defn four-of-a-kind? [hand]
  (= 4 (apply max (vals (frequencies (map rank hand))))))

(defn flush? [hand]
  (= 5 (apply max (vals (frequencies (map suit hand))))))

(defn full-house? [hand]
  (and (three-of-a-kind? hand) (= 2 (apply min (vals (frequencies (map rank hand)))))))

(defn two-pairs? [hand]
  (if (pair? hand) 
    (= 2 
       (get (frequencies (vals (frequencies (map rank hand)))) 2))
    false))

(defn straight? [hand]

  (let [sorted (sort (map rank hand))]
    (if (= (range (nth sorted 0) 
          (+ (nth sorted 0) 5))
	  sorted)
	true
	(= [2 3 4 5 14] sorted))))

(defn straight-flush? [hand]
  (and (straight? hand)(flush? hand) ))

(defn value [hand]
  (cond 
    (two-pairs? hand) 2
    (pair? hand) 1
    (four-of-a-kind? hand) 7
    (full-house? hand) 6
    (three-of-a-kind? hand) 3
    (straight-flush? hand) 8
    (straight? hand) 4
    (flush? hand) 5
    :else 0)
)
