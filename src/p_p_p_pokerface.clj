(ns p-p-p-pokerface)

(defn rank [card]
  (let [[r _] card]
    (if (Character/isDigit r)
      (Integer/valueOf (str r))
      (get {\T 10, \J 11, \Q 12, \K  13, \A 14} r))))

(defn suit [card]
  (let [[_ s] card]
    (str s)))

(defn pair? [hand]
  (contains? (set (vals (frequencies (map rank hand)))) 2))

(defn three-of-a-kind? [hand]
  (contains? (set (vals (frequencies (map rank hand)))) 3))

(defn four-of-a-kind? [hand]
  (contains? (set (vals (frequencies (map rank hand)))) 4))

(defn flush? [hand]
  (contains? (set (vals (frequencies (map suit hand)))) 5))

(defn full-house? [hand]
  (and (contains? (set (vals (frequencies (map rank hand)))) 2)
       (contains? (set (vals (frequencies (map rank hand)))) 3)))

(defn two-pairs? [hand]
  (==
   (count
    (filter (fn [x] (== x 2)) (vals (frequencies (map rank hand)))))
   2))

(defn straight? [hand]
  (let [ranks (set (keys (frequencies (map rank hand))))]
      (or (and (== 4 (- (apply max ranks) (apply min ranks))) (== 5 (count ranks)))
          (= ranks #{14 2 3 4 5}))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn value [hand]
  (cond (straight-flush? hand) 8
        (four-of-a-kind? hand) 7
        (full-house? hand) 6
        (flush? hand) 5
        (straight? hand) 4
        (three-of-a-kind? hand) 3
        (two-pairs? hand) 2
        (pair? hand) 1
        :else 0))

