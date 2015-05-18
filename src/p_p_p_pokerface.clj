(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rank _] card]
    (if (Character/isDigit rank)
      (Integer/valueOf (str rank))
      (get {\T 10, \J 11, \Q 12, \K 13, \A 14} rank))))

(defn suit [card]
  (let [[_ suit] card]
  (str suit)))

(defn max-freq [a-seq]
  (apply max (vals (frequencies a-seq))))

(defn pair? [hand]
  (= (max-freq (map rank hand)) 2))

(defn three-of-a-kind? [hand]
    (= (max-freq (map rank hand)) 3))

(defn four-of-a-kind? [hand]
    (= (max-freq (map rank hand)) 4))

(defn flush? [hand]
  (= (max-freq (map suit hand)) 5))

(defn sorted-freq [hand]
  (sort (vals (frequencies (map rank hand)))))

(defn full-house? [hand]
  (= (sorted-freq hand) [2 3]))

(defn two-pairs? [hand]
    (or (= (sorted-freq hand) [1 2 2])
      (= (sorted-freq hand) [1 4])))

(defn straight? [hand]
 (or (= (sort (keys (frequencies (map rank hand))))
      (range (apply min (map rank hand)) (+ 5 (apply min (map rank hand)))))
     (= (sort (keys (frequencies (replace {14 1} (map rank hand) ))))
      (range 1 6))))


(defn straight-flush? [hand]
  (and (flush? hand) (straight? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                   [two-pairs? 2]  [three-of-a-kind? 3]
                   [straight? 4]   [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}]

(apply max (map second (filter #((first %) hand) checkers)))))
