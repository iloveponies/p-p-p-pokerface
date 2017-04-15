(ns p-p-p-pokerface)

(def ranks {\T 10, \J 11, \Q 12, \K 13, \A 14})

(defn rank [card]
  (let [[arvo _] card]
  (if(Character/isDigit arvo)
    (Integer/valueOf(str arvo))
    (get ranks arvo))))

(defn suit [card]
  (let [[_ vari] card]
    (str vari)))

(defn pair? [hand]
  (contains? (set(vals(frequencies(map rank hand)))) 2))

(defn three-of-a-kind? [hand]
  (contains? (set(vals(frequencies(map rank hand)))) 3))

(defn four-of-a-kind? [hand]
 (contains? (set(vals(frequencies(map rank hand)))) 4))

(defn flush? [hand]
 (== 1 (count (set(vals(frequencies(map suit hand)))))))

(defn full-house? [hand]
  (and(pair? hand) (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  (or (four-of-a-kind? hand) (= (seq [1 2 2]) (sort (vals (frequencies (map rank hand)))))))

(defn straight? [hand]
  (let [ranks (sort (map rank hand))]
    (or (= (range (apply min ranks) (+ 5 (apply min ranks)))
           ranks)
       (= '(2 3 4 5 14)
          ranks))))

(defn straight-flush? [hand]
 (and (flush? hand) (straight? hand)))

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

