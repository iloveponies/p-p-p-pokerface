(ns p-p-p-pokerface)

(defn rank [card]
  (let [[fst _] card]
    (if (Character/isDigit fst)
      (Integer/valueOf (str fst))
      (get {\T 10, \J 11, \Q 12, \K 13, \A 14} fst))))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn pair? [hand]
  (= 2 (apply max (vals (frequencies (map rank hand))))))

(defn three-of-a-kind? [hand]
  (= 3 (apply max (vals (frequencies (map rank hand))))))

(defn four-of-a-kind? [hand]
  (= 4 (apply max (vals (frequencies (map rank hand))))))

(defn flush? [hand]
  (= 5 (apply max (vals (frequencies (map suit hand))))))

(defn full-house? [hand]
  (= (sort (vals (frequencies (map rank hand)))) (range 2 4)))

(defn two-pairs? [hand]
  (or (= (sort (vals (frequencies (map rank hand)))) (seq [1 2 2]))
    (= (sort (vals (frequencies (map rank hand)))) (seq [1 4]))))


(defn straight? [hand]
  (or (= (sort (map rank hand)) (range (apply min (sort (map rank hand))) (+ (apply min (sort (map rank hand))) 5)))
  (= (sort (replace {14 1} (map rank hand))) (range (apply min (sort (replace {14 1} (map rank hand)))) (+ (apply min (sort (replace {14 1} (map rank hand)))) 5)))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  true) ; All hands have a high card.

(defn value [hand]
  (let [checkerts #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}]
  (apply max (map second (filter first (map #(vector ((first %) hand) (second %)) checkerts))))))
