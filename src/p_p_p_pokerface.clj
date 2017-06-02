(ns p-p-p-pokerface)

(defn rank [card]
  (let [[r _] card]
    (if (Character/isDigit r) (Integer/valueOf (str r))
      (let [ranks {\T 10, \J 11, \Q 12, \K 13, \A 14}]
        (ranks r)))))


(defn suit [card]
  (let [[_ s] card]
    (str s)))


(defn pair? [hand]
  (= 2 (apply max (vals (frequencies (map rank hand))))))


(defn three-of-a-kind? [hand]
  (and (= 3 (apply max (vals (frequencies (map rank hand)))))
       (not (= 2 (apply min (vals (frequencies (map rank hand))))))))

(defn four-of-a-kind? [hand]
  (= 4 (apply max (vals (frequencies (map rank hand))))))

(defn flush? [hand]
  (= 5 (apply max (vals (frequencies (map suit hand))))))

(defn full-house? [hand]
  (and (= 3 (apply max (vals (frequencies (map rank hand)))))
       (= 2 (apply min (vals (frequencies (map rank hand)))))))

(defn two-pairs? [hand]
  (cond (four-of-a-kind? hand) true
        (three-of-a-kind? hand) false
        (= 2 (apply max (vals (frequencies (vals (frequencies (map rank hand))))))) true
        :else false))


(defn straight? [hand]
  (let [ranks (sort (map rank hand))
        low (apply min ranks)
        high (apply max ranks)
        oneup (sort (replace {14 1} ranks))
        newlow (apply min oneup)
        newhigh (apply max oneup)]
    (cond
      (= ranks (range low (+ 1 high))) true
      (= oneup (range newlow (+ 1 newhigh))) true
      :else false)))


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





