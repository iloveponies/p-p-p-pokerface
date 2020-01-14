(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rank _] card
        ranks {\T 10, \J 11, \Q 12, \K 13, \A 14}]
    (cond
      (Character/isDigit rank) (Integer/valueOf (str rank))
      :else                    (ranks rank))))

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
  (= [5] (vals (frequencies (map suit hand)))))

(defn full-house? [hand]
  (let [freqs (vals (frequencies (map rank hand)))]
    (and
      (= 3 (apply max freqs))
      (= 2 (apply min freqs)))))

(defn two-pairs? [hand]
  (let [freqs (vals (frequencies (map rank hand)))
        pair? (fn [freq] (= 2 freq))]
    (= 2 (count (filter pair? freqs)))))

(defn straight? [hand]
  (let [sorted-ranks (sort (map rank hand))
        lowest (apply min sorted-ranks)]
    (or
      (= (range lowest (+ 5 lowest)) sorted-ranks)
      (let [sorted-ranks-low-ace (sort (replace {14 1} sorted-ranks))]
        (= (range 1 6) sorted-ranks-low-ace)))))


(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn value [hand]
  (cond
    (straight-flush?  hand) 8
    (four-of-a-kind?  hand) 7
    (full-house?      hand) 6
    (flush?           hand) 5
    (straight?        hand) 4
    (three-of-a-kind? hand) 3
    (two-pairs?       hand) 2
    (pair?            hand) 1
    :else                   0))
