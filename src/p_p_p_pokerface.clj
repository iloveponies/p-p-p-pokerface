(ns p-p-p-pokerface)

(defn rank [card]
  (let [ [rank _ ] card]
      (if (Character/isDigit rank)
        (Integer/valueOf (str rank))
        (get {\T 10, \J 11, \Q 12, \K 13, \A 14} rank))))

(defn suit [card]
  (let [[_ suit] card]
      (str suit)))

(defn pair? [hand]
  (if (<= 2 (apply max (vals (frequencies (map rank hand))))) true false))

(defn three-of-a-kind? [hand]
  (if (<= 3 (apply max (vals (frequencies (map rank hand))))) true false))

(defn four-of-a-kind? [hand]
  (if (<= 4 (apply max (vals (frequencies (map rank hand))))) true false))

(defn flush? [hand]
  (if (= 5 (apply max (vals (frequencies (map suit hand))))) true false))


(defn full-house? [hand]
  (if (= [2 3] (into [] (sort (vals (frequencies (map rank hand)))))) true false))

(defn two-pairs? [hand]
  (if (= [1 2 2] (into [] (sort (vals (frequencies (map rank hand)))))) true false))

(defn minm [hand]
    (apply min (map rank hand)))

(defn maxm [hand]
    (apply max (map rank hand)))

(defn melkosuora [hand]
  (if (= [(minm hand) (+ (minm hand) 1) (+ (minm hand) 2) (+ (minm hand) 3) (+ (minm hand) 4)] (into [] (sort (map rank hand)))) true false))

(defn pieniassasuora [hand]
  (if (= [(minm hand) (+ (minm hand) 1) (+ (minm hand) 2) (+ (minm hand) 3) 14] (into [] (sort (map rank hand)))) true false))

(defn straight? [hand]
  (if (= 14 (maxm hand))
         (if (pieniassasuora hand) true (melkosuora hand)) (melkosuora hand)))

(defn straight-flush? [hand]
  (if (and (straight? hand) (flush? hand)) true false))

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
    :else 0
    ))
