(ns p-p-p-pokerface)

(def replacements { \T 10, \J 11, \Q 12, \K 13, \A 14 })

(defn rank [card]
  (let [[r _] card]
    (if (Character/isDigit r)
      (Integer/valueOf (str r)) (replacements r))))

(defn suit [card]
  (let [[_ s] card]
    (str s)))

(defn pair? [hand]
  (let [rank-hand (map rank hand)]
    (== (apply max (vals (frequencies rank-hand))) 2)))

(defn three-of-a-kind? [hand]
  (let [rank-hand (map rank hand)]
    (== (apply max (vals (frequencies rank-hand))) 3)))

(defn four-of-a-kind? [hand]
  (let [rank-hand (map rank hand)]
    (== (apply max (vals (frequencies rank-hand))) 4)))

(defn flush? [hand]
  (let [suit-hand (map suit hand)]
    (== (apply max (vals (frequencies suit-hand))) 5)))

(defn full-house? [hand]
  (let [rank-hand (map rank hand)]
    (= (sort (vals (frequencies rank-hand))) [2,3])))

(defn two-pairs? [hand]
  (let [rank-hand (map rank hand)]
    (or (= (sort (vals (frequencies rank-hand))) [1,2,2])
        (= (sort (vals (frequencies rank-hand))) [1,4]))))

(defn straight? [hand]
  (let [rank-hand-sorted (sort(map rank hand))
        max-val (apply max rank-hand-sorted)
        min-val (apply min rank-hand-sorted)]
    (or (= rank-hand-sorted (range min-val (+ max-val 1)))
        (= rank-hand-sorted [2,3,4,5,14]))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn value [hand]
  (cond
   (and (pair? hand) (not (two-pairs? hand))) 1
   (and (two-pairs? hand) (not (four-of-a-kind? hand))) 2
   (and (three-of-a-kind? hand) (not (full-house? hand))) 3
   (and (straight? hand) (not (flush? hand))) 4
   (and (flush? hand) (not (straight? hand))) 5
   (full-house? hand) 6
   (four-of-a-kind? hand) 7
   (straight-flush? hand) 8
   :else 0))