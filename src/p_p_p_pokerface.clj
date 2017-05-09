(ns p-p-p-pokerface)

(defn rank [card]
  (let [[fst _] card]
    (if (Character/isDigit fst)
        (Integer/valueOf (str fst))
        (get {\T 10 \J 11 \Q 12 \K 13 \A 14} fst))))

(defn suit [card]
  (str (let [[_ snd] card]
         snd)))

(defn pair? [hand]
  (> (apply max (vals (frequencies (map rank hand)))) 1))

(defn three-of-a-kind? [hand]
  (> (apply max (vals (frequencies (map rank hand)))) 2))

(defn four-of-a-kind? [hand]
  (> (apply max (vals (frequencies (map rank hand)))) 3))

(defn flush? [hand]
  (> (apply max (vals (frequencies (map suit hand)))) 4))

(defn full-house? [hand]
  (= [2 3] (sort (vals (frequencies (map rank hand))))))

(defn two-pairs? [hand]
  (or (= [1 2 2] (sort (vals (frequencies (map rank hand)))))
      (= [2 3] (sort (vals (frequencies (map rank hand)))))))

(defn straight? [hand]
  (if (or (= (range (apply min (map rank hand)) (+ (apply min (map rank hand)) 5))
             (sort (map rank hand)))
          (= [2 3 4 5 14] (sort (map rank hand))))
    true
    false))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                   [two-pairs? 2]  [three-of-a-kind? 3]
                   [straight? 4]   [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}]
    (apply max (map (fn [[f score]]
                      (if (f hand)
                        score
                        0)) checkers))))
