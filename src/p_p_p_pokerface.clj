(ns p-p-p-pokerface)

(def values {\T 10, \J 11, \Q 12, \K 13, \A 14})

(defn rank [card]
  (let [[rank _] card]
    (if
      (Character/isDigit rank) (Integer/valueOf (str rank))
      (get values rank))))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn high-card? [hand]
  true)

(defn pair? [hand]
  (>= (apply max (vals (frequencies (map rank hand)))) 2))

(defn three-of-a-kind? [hand]
  (>= (apply max (vals (frequencies (map rank hand)))) 3))

(defn four-of-a-kind? [hand]
  (= (apply max (vals (frequencies (map rank hand)))) 4))

(defn flush? [hand]
  (= (apply max (vals (frequencies (map suit hand)))) 5))

(defn full-house? [hand]
  (= (sort (vals (frequencies (map rank hand)))) [2 3]))

(defn two-pairs? [hand]
  (= (sort (vals (frequencies (map rank hand)))) [1 2 2]))

(defn straight? [hand]
  (let [straight-hand
        (fn [start]
          (range start (+ start 5)))
        sorted-hand
          (seq (sort (map rank hand)))
        ace-sorted-hand
          (seq (sort (replace {14 1} (map rank hand))))]
  (or
    (= sorted-hand (straight-hand (first sorted-hand)))
    (= ace-sorted-hand (straight-hand (first ace-sorted-hand))))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn value [hand]
  (let [checkers #{[high-card? 0] [pair? 1] [two-pairs? 2] [three-of-a-kind? 3]
                  [straight? 4] [flush? 5] [full-house? 6] [four-of-a-kind? 7]
                  [straight-flush? 8]}
        handvalue (fn [f] (second f))]
  (apply max
     (filter max
        (map (fn [f]
            (if ((first f) hand) (handvalue f)))
         checkers)))))
