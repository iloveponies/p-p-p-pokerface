(ns p-p-p-pokerface)

(defn rank [card]
  (let[repl {\T 10, \J 11, \Q 12, \K 13, \A 14}
       [fst snd] card]
    (if (not (Character/isDigit fst)) (get repl fst) (Integer/valueOf (str fst)))))

(defn suit [card]
  (let [[fst snd] card]
    (str snd)))

(defn pair? [hand]
  (let [sort (sort (vals (frequencies (map #(first %) hand))))]
    (= 2 (some #{2} sort))))

(defn three-of-a-kind? [hand]
  (= (first (vals (frequencies (map #(first %) hand)))) 3))

(defn four-of-a-kind? [hand]
  (= (first (vals (frequencies (map #(first %) hand)))) 4))

(defn flush? [hand]
  (= (first (vals (frequencies (map #(second %) hand)))) 5))

(defn full-house? [hand]
  (and
  (= (first (vals (sort (frequencies (map #(first %) hand))))) 3)
  (= (second (vals (sort (frequencies (map #(first %) hand))))) 2)))

(defn two-pairs? [hand]
  (let [seq (sort (vals (frequencies (map #(first %) hand))))]
    (or (= '(1 2 2) seq) (= '(2 3) seq) (= '(1 4) seq))))

(defn straight? [hand]
  (let [sorted (sort (map first hand))
        ace (if (= (apply min (map int sorted)) 50) (sort (map int (replace {\A 49} sorted))) (sort (map int (replace {\T 58, \J 59, \Q 60, \K 61 \A 62} sorted))))
        min-value (apply min (map int ace))]
    (= ace (take 5 (iterate inc min-value)))))

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
