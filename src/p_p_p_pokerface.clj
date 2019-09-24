(ns p-p-p-pokerface)




(defn rank [card]
  (let [[fst _] card]
    (if (Character/isDigit fst) (Integer/valueOf (str fst)) (get {\T 10, \J 11, \Q 12,  \K 13, \A 14} fst))))


(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn pair? [hand]
  (> (apply max (vals (frequencies(map rank hand)))) 1 ))

(defn three-of-a-kind? [hand]
  (> (apply max (vals (frequencies(map rank hand)))) 2 ))

(defn four-of-a-kind? [hand]
  (> (apply max (vals (frequencies(map rank hand)))) 3 ))

(defn flush? [hand]
  (apply = (map suit hand)))

(defn full-house? [hand]
 (and (contains?(set(vals(frequencies(map rank hand)))) 2) (contains?(set(vals(frequencies(map rank hand)))) 3)))

(defn two-pairs? [hand]
  (= (get(frequencies(vals(frequencies(map rank hand))))2) 2))

(defn straight? [hand]
  (cond
   (= (sort(map rank hand)) [2 3 4 5 14]) true
   (= (sort(map rank hand)) [2 3 4 5 6]) true
   (= (sort(map rank hand)) [3 4 5 6 7]) true
   (= (sort(map rank hand)) [4 5 6 7 8]) true
   (= (sort(map rank hand)) [5 6 7 8 9]) true
   (= (sort(map rank hand)) [6 7 8 9 10]) true
   (= (sort(map rank hand)) [7 8 9 10 11]) true
   (= (sort(map rank hand)) [8 9 10 11 12]) true
   (= (sort(map rank hand)) [9 10 11 12 13]) true
   (= (sort(map rank hand)) [10 11 12 13 14]) true
   :else false))

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
