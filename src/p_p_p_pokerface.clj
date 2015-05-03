(ns p-p-p-pokerface)

(defn rank [card]
  (let [ [rank _] card char_map {\T 10, \J 11, \Q 12, \K 13, \A 14} ]
    (if (Character/isDigit rank)
      (Integer/valueOf (str rank))
      (get char_map rank))))

(defn suit [card]
  (let [ [_ suit] card ]
    (str suit)))

(defn pair? [hand]
  (>= (apply max (vals (frequencies (map rank hand)))) 2))

(defn three-of-a-kind? [hand]
  (== (apply max (vals (frequencies (map rank hand)))) 3))

(defn four-of-a-kind? [hand]
  (== (apply max (vals (frequencies (map rank hand)))) 4))

(defn flush? [hand]
  (apply = (map second hand)))

(defn full-house? [hand]
  (let [ ranks (mapv (fn [card] (rank card)) hand) ]
    (= (sort (vals (frequencies ranks))) (seq [2 3]))))

(defn two-pairs? [hand]
  (or
    (= 2 (get (frequencies (vals (frequencies (map rank hand)))) 2))
    (= 1 (get (frequencies (vals (frequencies (map rank hand)))) 4))))

(defn straight? [hand]
  (let [ sorted (sort (map rank hand)) smallest (first sorted)
        alt (sort (replace {14 1} sorted)) alt-smallest (first alt) ]
    (or
      (= sorted (range smallest (+ smallest 5)))
      (= alt (range alt-smallest (+ alt-smallest 5))))))

(defn straight-flush? [hand]
  (and
    (straight? hand)
    (flush? hand)))

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
