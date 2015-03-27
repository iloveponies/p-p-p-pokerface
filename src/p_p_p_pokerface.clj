(ns p-p-p-pokerface)

(defn rank [card]
  (let [replacements {\T 10, \J 11, \Q 12, \K 13, \A 14}
        [rank _] card]
    (if (Character/isDigit rank)
      (Integer/valueOf (str rank))
      (replacements rank))))

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
  (apply = (map suit hand)))

(defn full-house? [hand]
  (= [2 3]
     (sort (vals (frequencies (map rank hand))))))

(defn two-pairs? [hand]
  (let [freqs (sort (vals (frequencies (map rank hand))))]
    (or (= freqs [1 2 2])
        (= freqs [1 4]))))

(defn straight? [hand]
  (let [high (sort (map rank hand))
        low (sort (replace {14 1} high))]
    (or (= high (range (first high)
                       (+ 1 (last high))))
        (= low (range (first low)
                      (+ 1 (last low)))))))

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
