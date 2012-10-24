(ns p-p-p-pokerface)

(defn rank [card]
  (let [[r _] card
        values {\T 10, \J 11, \Q 12, \K 13, \A 14}]
    (if (Character/isDigit r)
      (Integer/valueOf (str r))
      (values r))))

(defn suit [[_ s]]
  (str s))

(defn rank-frequencies [hand]
  (vals (frequencies (map rank hand))))

(defn pair? [hand]
  (contains? (set (rank-frequencies hand)) 2))

(defn three-of-a-kind? [hand]
  (contains? (set (rank-frequencies hand)) 3))
  
(defn four-of-a-kind? [hand]
  (contains? (set (rank-frequencies hand)) 4))

(defn flush? [hand]
  (= 1 (count (set (map suit hand)))))

(defn full-house? [hand]
  (= #{3 2} (set (rank-frequencies hand))))

(defn two-pairs? [hand]
  (or (<= 2 (count (filter 
                     #(= 2 %) 
                     (rank-frequencies hand))))
      (four-of-a-kind? hand)))

(defn straight? [hand]
  (let [sorted-hand (sort (map rank hand))
        alt-hand (sort (replace {14 1} sorted-hand))
        reference (range (apply min sorted-hand) 
                         (+ 1 (apply max sorted-hand)))
        alt-reference (range (apply min alt-hand)
                             (+ 1 (apply max alt-hand)))]
        (or (= sorted-hand reference)
            (= alt-hand alt-reference))))

(defn straight-flush? [hand]
  (and (straight? hand)
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

