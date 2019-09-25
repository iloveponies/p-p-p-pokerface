(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rank _] card]
  (def replacements {\T 10, \J 11, \Q 12, \K 13, \A 14})
  (cond
    (Character/isDigit rank) (Integer/valueOf (str rank))
    :else (replacements rank))))

(defn suit [card]
  (let [[_ suit] card]
  (str suit)))

(defn pair? [hand]
  (= 1 (count (filter #(= 2 %) (vals (frequencies (map rank hand)))))))

(defn three-of-a-kind? [hand]
  (= 1 (count (filter #(= 3 %) (vals (frequencies (map rank hand)))))))

(defn four-of-a-kind? [hand]
  (= 1 (count (filter #(= 4 %) (vals (frequencies (map rank hand)))))))

(defn flush? [hand]
  (= 5 (apply max (vals (frequencies (map suit hand))))))

(defn full-house? [hand]
  (= [3 2] (vals (frequencies (map rank hand)))))

(defn two-pairs? [hand]
  (= 2 (count (filter #(= 2 %) (vals (frequencies (map rank hand)))))))

(defn straight? [hand]
  (let [sorted-hand (sort (map rank hand))
        min-sorted-hand (apply min sorted-hand)
        ace-sorted-hand (sort (replace {14 1} sorted-hand))
        min-ace-sorted-hand (apply min ace-sorted-hand)]
  (or (= sorted-hand (range min-sorted-hand (+ min-sorted-hand 5)))
      (= ace-sorted-hand (range min-ace-sorted-hand (+ min-ace-sorted-hand 5))))))

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
