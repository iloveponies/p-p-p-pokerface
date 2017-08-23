(ns p-p-p-pokerface)

(defn rank [card]
  (let [[fst _] card]
    (if (Character/isDigit fst)
      (Integer/valueOf (str fst))
      ({\A 14, \T 10, \J 11, \Q 12, \K 13} fst))))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn pair? [hand] (if (< 1 (apply max (vals (frequencies (map rank hand))))) true false))

(defn three-of-a-kind? [hand] (if (< 2 (apply max (vals (frequencies (map rank hand))))) true false))

(defn four-of-a-kind? [hand] (if (< 3 (apply max (vals (frequencies (map rank hand))))) true false))

(defn flush? [hand] (if (== 5 (first (vals (frequencies (map suit hand))))) true false))

(defn full-house? [hand] (let [values (vals (frequencies (map rank hand)))]
  (if (and
        (== 2 (count values))
        (or (== 2 (first values)) (== 3 (first values))))
    true
    false)))

(defn two-pairs? [hand]
  (let [values (vals (frequencies (map rank hand)))]
    (or (and (== 3 (count values))
           (== (apply max values) 2)
           (== (apply min values) 1))
        (and (== 2 (count values))
             (== (apply max values) 4)))))

(defn straight? [hand]
  (let [values (sort (map rank hand))
        low-ace (sort (replace {14 1} values))]
    (or
      (= values (range (first values) (+ (first values) 5)))
      (= low-ace (range (first low-ace) (+ (first low-ace) 5))))))

(defn straight-flush? [hand] (and (straight? hand) (== 5 (first (vals (frequencies (map suit hand)))))))

(defn value [hand]
  (let [high-card? (fn [x] true)
       checkers #{
                   [high-card? 0]
                   [pair? 1]
                   [two-pairs? 2]
                   [three-of-a-kind? 3]
                   [straight? 4]
                   [flush? 5]
                   [full-house? 6]
                   [four-of-a-kind? 7]
                   [straight-flush? 8]}]
    (apply max (map second (filter (fn [x] ((first x) hand)) (map (fn [x] x) checkers))))))
