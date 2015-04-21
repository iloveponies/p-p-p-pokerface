(ns p-p-p-pokerface)

(defn rank [[fst _]]
  (if (Character/isDigit fst)
    (Integer/valueOf (str fst))
    (get {\T 10 \J 11 \Q 12 \K 13 \A 14} fst)))

(defn suit [[_ snd]]
  (str snd))

(defn pair? [hand]
  (let [max-freq (apply max(vals (frequencies (map rank hand))))]
    (if (>= max-freq 2)
      true
      false)))

(defn three-of-a-kind? [hand]
  (let [max-freq (apply max(vals (frequencies (map rank hand))))]
    (if (>= max-freq 3)
      true
      false)))

(defn four-of-a-kind? [hand]
  (let [max-freq (apply max(vals (frequencies (map rank hand))))]
    (if (>= max-freq 4)
      true
      false)))

(defn flush? [hand]
  (let [max-freq (apply max(vals (frequencies (map suit hand))))]
    (if (= max-freq 5)
      true
      false)))

(defn full-house? [hand]
  (let [sorted-values-freq (sort (vals (frequencies (map rank hand))))]
    (if (= sorted-values-freq [2 3])
      true
      false)))

(defn two-pairs? [hand]
  (let [sorted-values-freq (sort (vals (frequencies (map rank hand))))]
    (cond
      (= sorted-values-freq [1 2 2]) true
      (= sorted-values-freq [1 4]) true
      :else false)))

(defn straight? [hand]
  ;sorted-values from min to max
  ;sorted-values-low-ace ace count as a one
  (let [sorted-values (sort (map rank hand))
        min-value (apply min sorted-values)
        sorted-values-low-ace (sort (replace {14 1} (map rank hand)))
        min-value-low-ace (apply min sorted-values-low-ace)]
    (cond
      (= sorted-values (range min-value (+ min-value 5))) true
      (= sorted-values-low-ace (range min-value-low-ace (+ min-value-low-ace 5))) true
      :else false)))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn value [hand]
  (let [high-card? (fn [hand] true)
        checkers #{[high-card? 0]  [pair? 1]
                   [two-pairs? 2]  [three-of-a-kind? 3]
                   [straight? 4]   [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}
        values (map (fn [[function score]]
                      (if (function hand)
                        score
                        0))
                    checkers)]
    (apply max values)))
