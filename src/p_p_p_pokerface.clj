(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rank _] card
        replacement {\T 10, \J 11, \Q 12, \K 13, \A 14}]
    (if (Character/isDigit rank)
      (Integer/valueOf (str rank))
      (get replacement rank))))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn pair? [hand]
  (contains? (set(vals(frequencies (map rank hand)))) 2))

(defn three-of-a-kind? [hand]
  (contains? (set(vals(frequencies (map rank hand)))) 3))

(defn four-of-a-kind? [hand]
  (contains? (set(vals(frequencies (map rank hand)))) 4))

(defn flush? [hand]
  (contains? (set(vals(frequencies (map suit hand)))) 5))

(defn full-house? [hand]
  (=(sort(vals(frequencies(map rank hand))))(range 2 4)))

(defn two-pairs? [hand]
  (=(sort(vals(frequencies(map rank hand))))[1,2,2]))

(defn straight? [hand]
  (let [straightRanks? (fn [ranks]
                         (let [minR (apply min ranks)
                               maxR (apply max ranks)
                               diff (count (frequencies ranks))]
        (and (== diff 5)
             (== (- maxR minR) 4))))]
    (or (straightRanks? (map rank hand)) (straightRanks? (replace {14 1} (map rank hand))))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

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
