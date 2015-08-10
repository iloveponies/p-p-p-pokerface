(ns p-p-p-pokerface)

(def face-cards {\T 10, \J 11, \Q 12, \K 13, \A 14})

(defn rank [card]
  (let [[^char rank _] card]
    (cond (Character/isDigit rank)
          (Integer/valueOf (str rank))
          :else (get face-cards rank))))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn pair? [hand]
  (= 2 (apply max (vals (frequencies (map rank hand))))))

(defn three-of-a-kind? [hand]
  (= 3 (apply max (vals (frequencies (map rank hand))))))

(defn four-of-a-kind? [hand]
  (= 4 (apply max (vals (frequencies (map rank hand))))))

(defn flush? [hand]
  (= 5 (apply max (vals (frequencies (map suit hand))))))

(defn full-house? [hand]
  (and (= 3 (apply max (vals (frequencies (map rank hand))))) (= 2 (apply min (vals (frequencies
                                                                                      (map rank hand)))))))

(defn two-pairs? [hand]
  (or (four-of-a-kind? hand)) (= 2 (count (filter #(= 2 %) (vals (frequencies (sort (map rank hand)
                                                                                    ))))
                                          )))

(defn straight? [hand]
  (let [sorted-hand (sort (map rank hand))
        min (apply min sorted-hand)]
    (or (= sorted-hand (range min (+ min 5)))
        (= (sort (replace {14 1} sorted-hand)) (range 1 6)))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0] [pair? 1]
                   [two-pairs? 2] [three-of-a-kind? 3]
                   [straight? 4] [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}]
    (apply max (map second (filter #((first %) hand) checkers)))))
