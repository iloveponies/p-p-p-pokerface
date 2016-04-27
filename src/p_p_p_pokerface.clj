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
  (< 1 (apply max (vals (frequencies (map rank hand))))))

(defn three-of-a-kind? [hand]
  (< 2 (apply max (vals (frequencies (map rank hand))))))

(defn four-of-a-kind? [hand]
  (< 3 (apply max (vals (frequencies (map rank hand))))))

(defn flush? [hand]
  (= 5 (apply max (vals (frequencies (map suit hand))))))

(defn full-house? [hand]
  (= (sort (vals (frequencies (map rank hand)))) (range 2 4)))

(defn two-pairs? [hand]
  (or
   (= [1 2 2] (sort (vals (frequencies (map rank hand)))))
   (= [1 4] (sort (vals (frequencies (map rank hand)))))))

(defn smallest-rank [hand]
  (apply min (sort (keys (frequencies (map rank hand))))))

(defn replace-ace [hand]
  (if (= (smallest-rank hand) 2)
    {14 1}
    {14 14}))

(defn straight? [hand]
  (and
   (= 1 (apply max (sort (vals (frequencies (map rank hand))))))
   (= (+ (apply min (sort (replace (replace-ace hand) (map rank hand)))) 4)
      (apply max (sort (replace (replace-ace hand) (map rank hand)))))))

(defn high-card? [hand]
  true)

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn value [hand]
  (let [checkers #{[high-card? 0]
                   [pair? 1]
                   [two-pairs? 2]
                   [three-of-a-kind? 3]
                   [straight? 4]
                   [flush? 5]
                   [full-house? 6]
                   [four-of-a-kind? 7]
                   [straight-flush? 8]}]
    (apply max (map second (filter (fn [x] ((first x) hand)) checkers)))))
