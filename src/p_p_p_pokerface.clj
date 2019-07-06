(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rank _] card
        values   {\T 10, \J 11, \Q 12, \K 13, \A 14}]
    (if (Character/isDigit rank)
      (Integer/valueOf (str rank))
      (values rank))))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn counts [size rank-or-suit hand]
  (count (filter #(= size %) (vals (frequencies (map rank-or-suit hand))))))

(defn in-hand? [size rank-or-suit hand]
  (< 0 (counts size rank-or-suit hand)))

(defn rank-in-hand? [size hand]
  (in-hand? size rank hand))

(defn pair? [hand]
  (rank-in-hand? 2 hand))

(defn three-of-a-kind? [hand]
  (rank-in-hand? 3 hand))

(defn four-of-a-kind? [hand]
  (rank-in-hand? 4 hand))

(defn flush? [hand]
  (in-hand? 5 suit hand))

(defn full-house? [hand]
  (and
    (pair? hand)
    (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  (= 2 (counts 2 rank hand)))

(defn straight? [hand]
  (let [high-ace (sort (map rank hand))
        low-ace  (sort (replace {14 1} high-ace))
        straight (fn [min] (range min (+ min 5)))]
    (or
     (= high-ace (straight (apply min high-ace)))
     (= low-ace (straight (apply min low-ace))))))

(defn straight-flush? [hand]
  (and
   (straight? hand)
   (flush? hand)))

(defn value [hand]
  (cond
   (straight-flush? hand)  8
   (four-of-a-kind? hand)  7
   (full-house? hand)      6
   (flush? hand)           5
   (straight? hand)        4
   (three-of-a-kind? hand) 3
   (two-pairs? hand)       2
   (pair? hand)            1
   :else                   0))
