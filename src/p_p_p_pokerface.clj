(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rank _] card
        values {\A 14, \K 13, \Q 12, \J 11, \T 10}]
    (if (Character/isDigit rank)
      (Integer/valueOf (str rank))
      (get values rank))))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn groups? [hand n]
  (let [freq (vals (frequencies (map rank hand)))
        fun (fn [x] (== x n))]
    (> (count (filter fun freq)) 0)))

(defn pair? [hand]
  (groups? hand 2))

(defn three-of-a-kind? [hand]
  (groups? hand 3))

(defn four-of-a-kind? [hand]
  (groups? hand 4))

(defn flush? [hand]
  (let [ suit-seq (map suit hand)]
    (apply = suit-seq)))

(defn full-house? [hand]
  (let [three (three-of-a-kind? hand)
        pair (pair? hand)]
    (and three pair)))

(defn two-pairs? [hand]
  (let [freq (vals (frequencies (map rank hand)))
        fun (fn [x] (== x 2))]
    (== (count (filter fun freq)) 2)))

(defn straight? [hand]
  (let [hand-vals (map rank hand)
        alt-vals (replace {14 1} hand-vals)
        fun (fn [x] (= x (range (apply min x) (+ (apply min x) 5))))]
    (or (fun (sort hand-vals)) (fun (sort alt-vals)))))

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
    :else 0 ))
